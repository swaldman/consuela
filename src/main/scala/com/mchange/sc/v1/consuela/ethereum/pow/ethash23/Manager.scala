/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import com.mchange.sc.v1.consuela._;
import conf.Config;
import ethereum._;
import ethereum.specification.Types.Unsigned64;

import com.mchange.sc.v1.log.MLevel._;

import com.mchange.sc.v3.failable.logging._;

import scala.concurrent.Future;
import scala.concurrent.ExecutionContext.Implicits.global;
import scala.collection._;
import scala.util.{Success,Failure}

import scala.annotation.tailrec;

object Manager {
  private implicit lazy val logger = mlogger( this );

  final object Light                 extends Light( Implementation.Default );
  final object FullManualCaching     extends Full ( Implementation.Default );
  final object FullStochasticCaching extends Full ( Implementation.Default ) with StochasticNextCaching;

  val DoubleDag      = Config.EthereumPowEthash23ManagerDoubleDag;
  val InMemGenFactor = Config.EthereumPowEthash23DagInMemGenFactor;

  val AvailableMemory = Runtime.getRuntime.totalMemory;

  object StochasticNextCaching {
    val ExpectedEnsureNextCacheChecksPerEpoch  = 10;
    val EnsureNextCacheChecksPerEpochThreshold = 1d / ExpectedEnsureNextCacheChecksPerEpoch; // we check if we are beneath this Threshold
  }
  trait StochasticNextCaching extends Abstract {
    import StochasticNextCaching._

    //MT: protected by its own lock
    //    ( i.e. midpersisteEpochs.synchronized { ... } )
    private[this] val midpersistEpochs = mutable.Set.empty[Long];

    override protected def prepareForEpoch( DesiredEpoch : Long ) : EpochRecord = {
      try super.prepareForEpoch( DesiredEpoch ) finally stochasticEnsureNextEpochCached( DesiredEpoch )
    }

    private[this] def stochasticEnsureNextEpochCached( currentEpochNumber : Long ) : Unit = {
      if (scala.math.random < EnsureNextCacheChecksPerEpochThreshold) {
        ensureCached( currentEpochNumber + 1 );
      }
    }

    private[this] def unmidpersistEpoch( epochNumber : Long ) : Unit = midpersistEpochs.synchronized( midpersistEpochs -= epochNumber )

    private[this] def ensureCached( epochNumber : Long ) : Unit = {
      midpersistEpochs.synchronized {
        val midpersist = midpersistEpochs( epochNumber );
        if (! midpersist ) {
          midpersistEpochs += epochNumber;
          Future[Unit] {
            try cacheEpoch( epochNumber ) finally unmidpersistEpoch( epochNumber )
          }
        }
      }
    }

    override protected def buildDataset( epochNumber : Long, seed : Array[Byte], cache : implementation.Cache, fullSize : Long ) : BuiltDataset = {
      val bd = super.buildDataset( epochNumber, seed, cache, fullSize );
      if (! bd.cached ) {
        midpersistEpochs.synchronized {
          val midpersist = midpersistEpochs( epochNumber );
          if (! midpersist ) {
            midpersistEpochs += epochNumber;
            val t = new Thread {
              this.setDaemon( false );

              override def run : Unit = {
                try {
                  implementation.cacheDataset( seed, bd.dataset ).xwarn( s"Tried but failed to cache in-memory dataset for epoch number ${epochNumber} to ethash DAG file." )
                } finally {
                  unmidpersistEpoch( epochNumber )
                }
              }
            }
            t.start
          }
        }
        new BuiltDataset( bd.dataset, true );
      } else {
        bd
      }
    }
    private[this] def cacheEpoch( epochNumber : Long ) : Unit = {
      implementation.streamDagFileForEpochNumber( epochNumber ).xwarn(s"Failed to stream and cache ethash DAG file for epoch number ${epochNumber}.");
    }
  }

  abstract class Abstract( val implementation : Implementation ) extends Manager {
    protected final class EpochRecord( val epochNumber : Long, val seed : Array[Byte], val cache : implementation.Cache, val mbDataset : Option[implementation.Dataset] )
    protected final class BuiltDataset( val dataset : implementation.Dataset, val cached : Boolean )


    //MT: protected by this' lock
    private[this] var preparing  : Option[Long]        = None;
    private[this] var lastRecord : EpochRecord         = null; //so sue me, only for light clients unless DoubleDag is (unusually) set
    private[this] var record     : EpochRecord         = null; //so sue me
    private[this] val waiters    : mutable.Set[Thread] = mutable.Set.empty[Thread];

    protected val holdsDataset : Boolean;

    private def keepsLastRecord = (!holdsDataset) || DoubleDag;

    protected def prepareForBlock( blockNumber : Long ) : EpochRecord = {
      val DesiredEpoch = epochFromBlock( blockNumber );
      prepareForEpoch( DesiredEpoch );
    }

    protected def prepareForEpoch( DesiredEpoch : Long ) : EpochRecord = {
      try { this.synchronized( _prepareForEpoch( DesiredEpoch ) ); }
      catch { case ie : InterruptedException => throw new FailureDuringDAGAcquisition( ie ) }
    }

    //MT: Must be called by while holding this' lock
    @tailrec
    private def _prepareForEpoch( DesiredEpoch : Long ) : EpochRecord = {
      def oneOff( warningMessage : String ) : EpochRecord = {
        WARNING.log( warningMessage );
        this.buildEpochRecord( DesiredEpoch );
      }
      def lastRecordOrOneOff( warning : => String ) : EpochRecord = {
        if ( lastRecord != null && lastRecord.epochNumber == DesiredEpoch ) lastRecord else oneOff( warning )
      }
      def lastRecordOrOneOffPriorEpoch : EpochRecord = lastRecordOrOneOff {
        s"Ethash data for epoch ${DesiredEpoch} was requested, which is prior to the latest requested epoch ${record.epochNumber}. " +
        "This data will be generated for one-time use, which may impair application performance. Please mine/verify in order."
      }
      def lastRecordOrAlreadyPreparing : EpochRecord = lastRecordOrOneOff {
        s"Ethash data for epoch ${DesiredEpoch} was requested, while the application has already been instructed to prepare data for epoch ${preparing}. " +
        "This data will be generated for one-time use, which may impair application performance. Please mine/verify in order."
      }
      def startAsyncUpdate : Unit = {
        this.preparing = Some( DesiredEpoch );
        val fut = Future[EpochRecord] {
          this.buildEpochRecord( DesiredEpoch );
        }
        fut.onComplete { myTry =>
          myTry match {
            case Success( record ) => updateRecord( record ); // calls this.notifyAll()
            case Failure( t )      => panic( DesiredEpoch, t ); // interrupts wait()ers
          }
        }
      }

      if ( record == null || DesiredEpoch != record.epochNumber ) {
        this.preparing match {
          case Some( DesiredEpoch ) => {
            waiters += Thread.currentThread();
            this.wait();
            waiters -= Thread.currentThread();
            _prepareForEpoch( DesiredEpoch )
          }
          case None => {
            if ( record == null || DesiredEpoch > record.epochNumber ) {
              startAsyncUpdate;
              _prepareForEpoch( DesiredEpoch )
            } else {
              lastRecordOrOneOffPriorEpoch
            }
          }
          case Some( _ ) => lastRecordOrAlreadyPreparing
        }
      } else {
        record
      }
    }

    private def panic( failedEpochNumber : Long, t : Throwable ) : Unit = this.synchronized {
      SEVERE.log( s"While preparing to verify/mine for epoch ${failedEpochNumber}, an Exception occurred. Interrupting clients.", t );
      waiters.foreach( _.interrupt() )
    }

    private def updateRecord( record : EpochRecord ) : Unit = this.synchronized {
      this.lastRecord = if ( !this.keepsLastRecord ) null else this.record;
      this.record     = record;
      this.preparing  = None;
      this.notifyAll();
    }

    protected def buildEpochRecord( epochNumber : Long ) : EpochRecord = buildEpochRecord( epochNumber, this.holdsDataset );

    protected def buildEpochRecord( epochNumber : Long, includeDataset : Boolean ) : EpochRecord = {
      val seed      = Seed.getForEpoch( epochNumber );
      val cache     = implementation.mkCacheForEpoch( epochNumber );
      val mbDataset = if ( includeDataset ) Some( acquireDataset( epochNumber, seed, cache ) ) else None;
      new EpochRecord( epochNumber, seed, cache, mbDataset )
    }

    private def acquireDataset( epochNumber : Long, seed : Array[Byte], cache : implementation.Cache ) : implementation.Dataset = {
      val persistedDataset = implementation.loadDagFile( seed );
      persistedDataset.xwarn("Failed to load DAG file. Will create.").getOrElse {
        val fullSize = implementation.getFullSizeForEpoch( epochNumber );
        buildDataset( epochNumber, seed, cache, fullSize ).dataset
      }
    }

    // extra arguments for use by mixins
    protected def buildDataset( epochNumber : Long, seed : Array[Byte], cache : implementation.Cache, fullSize : Long ) : BuiltDataset = {
      //WARNING.log( s"fullSize: ${fullSize}, AvailableMemory: ${AvailableMemory}, (fullSize * InMemGenFactor) < AvailableMemory ): ${(fullSize * InMemGenFactor) < AvailableMemory}" );
      if ( (fullSize * InMemGenFactor) < AvailableMemory ) {
        val ds = implementation.calcDataset( cache, fullSize )
        new BuiltDataset( dataset=ds, cached=false )
      } else {
        val failableUnit = implementation.streamDagFileForEpochNumber( epochNumber, Some( seed ), Some( cache ), None );
        failableUnit.get; // will throw an Exception if failed
        new BuiltDataset( dataset=implementation.loadDagFile(seed).get , cached=true )
      }
    }

    protected def blockNumberFromHeader( header : EthBlock.Header ) : Long = {
      val proto = header.number.widen;
      if (! proto.isValidLong )
        throw new IllegalArgumentException("Implementation currently only handles blocknumbers within the range of a 64 but signed Long for now, your blocknumber ${proto} was not.");
      proto.toLong
    }
    def hashimoto( header : EthBlock.Header ) : Hashimoto = this.hashimoto( header, header.nonce );
  }
  //i8n is short for implementation, we had some compiler trouble with the shadowed variable
  abstract class Light( i8n : Implementation ) extends Abstract( i8n ) {
    val holdsDataset = false;

    def hashimoto( header : EthBlock.Header, nonce : Unsigned64 ) : Hashimoto = {
      val blockNumber = blockNumberFromHeader( header );
      val epochRecord = prepareForBlock( blockNumber );
      implementation.hashimotoLight( header, epochRecord.cache, nonce );
    }
  }
  //i8n is short for implementation, we had some compiler trouble with the shadowed variable
  abstract class Full( i8n : Implementation ) extends Abstract( i8n ) {
    val holdsDataset = true;

    def hashimoto( header : EthBlock.Header, nonce : Unsigned64 ) : Hashimoto = {
      val blockNumber = blockNumberFromHeader( header );
      val epochRecord = prepareForBlock( blockNumber );
      implementation.hashimotoFull( header, epochRecord.mbDataset.get, nonce );
    }
  }

  val AcqFailureMessage = {
    "A failure occurred during the acquisition DAG data for which this Thread was waiting. " +
    "Please review your logs prior to this Exception for more information."
  }
  class FailureDuringDAGAcquisition private[Manager] ( ie : InterruptedException ) extends EthereumException( AcqFailureMessage, ie );
}
trait Manager {
  def hashimoto( header : EthBlock.Header )                     : Hashimoto;
  def hashimoto( header : EthBlock.Header, nonce : Unsigned64 ) : Hashimoto;
}

