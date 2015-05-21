package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela.hash.{SHA3_256,SHA3_512};

import com.mchange.sc.v1.consuela._;
import conf.Config;
import ethereum._;
import ethereum.encoding._;
import ethereum.specification.Types.{Unsigned64,Unsigned256};

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import scala.collection._;

import scala.concurrent.Future;
import scala.concurrent.ExecutionContext.Implicits.global;

import scala.util.{Success,Failure}

import scala.annotation.tailrec;

import scala.reflect.ClassTag;

// is Long math good enough? (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...
// but when the dataset gets bigger than ~8GB, we'll exceed int array indices, and have to encode
// values into fully packed longs, rather than ints or longs representing unsigned ints as currently
// implements. (the current implementation will fail fast when this limit is exceeded.)

package object ethash23 {
  // implementing REVISION 23 of Ethash spec, defined https://github.com/ethereum/wiki/wiki/Ethash
  private implicit lazy val logger = MLogger( this );

  private[ethash23] final val WordBytes          = 1 << 2;  // 4
  private[ethash23] final val DatasetBytesInit   = 1 << 30; // 2 ** 30
  private[ethash23] final val DatasetBytesGrowth = 1 << 23; // 2 ** 23
  private[ethash23] final val CacheBytesInit     = 1 << 24; // 2 ** 24
  private[ethash23] final val CacheBytesGrowth   = 1 << 17; // 2 ** 17
  private[ethash23] final val CacheMultiplier    = 1 << 10; // 1024
  private[ethash23] final val EpochLength        = 30000;   // 30000
  private[ethash23] final val MixBytes           = 1 << 7;  // 128
  private[ethash23] final val HashBytes          = 1 << 6;  // 64
  private[ethash23] final val DatasetParents     = 1 << 8;  // 256
  private[ethash23] final val CacheRounds        = 3;       // 3
  private[ethash23] final val Accesses           = 1 << 6;  // 64

  private[ethash23] final val DoubleHashBytes = 2 * HashBytes; //128
  private[ethash23] final val DoubleMixBytes  = 2 * HashBytes; //256

  private[ethash23] final val HashBytesOverWordBytes = HashBytes / WordBytes; //16
  private[ethash23] final val MixBytesOverWordBytes  = MixBytes / WordBytes;  //32
  private[ethash23] final val MixBytesOverHashBytes  = MixBytes / HashBytes;  //2

  private[ethash23] final val Revision = 23; // the version of the Ethash spec we are implementing

  private[ethash23] final val FnvPrime = 0x01000193;

  private[ethash23] final val ProbablePrimeCertainty : Int = 8; //arbitrary, tune for performance...

  private[ethash23] final val RowWidth = 16; // four-byte Ints

  private[ethash23] implicit val SeedPrimer = Seed.Primer(
    Config.EthereumPowEthash23SeedPrimerEpochNumber,
    SHA3_256.withBytes(Config.EthereumPowEthash23SeedPrimerValue.decodeHex)
  );

  private[ethash23] implicit final class PlusModInt( val i : Int ) extends AnyVal {
    def +%( other : Int ) = scala.math.abs( i % other )
  }
  private[ethash23] implicit final class PlusModLong( val l : Long ) extends AnyVal {
    def +%( other : Int ) : Long  = scala.math.abs( l % other )
    def +%( other : Long ) : Long = scala.math.abs( l % other )
  }

  def epochFromBlock( blockNumber : Long ) : Long = ( blockNumber / EpochLength )
  def blocksRemainingInEpoch( blockNumber : Long ) : Long = EpochLength - ( blockNumber % EpochLength )

  case class Hashimoto( val mixDigest : immutable.Seq[Byte], val result : Unsigned256 ) {
    override def toString : String = s"Hashimote(mixDigest=${mixDigest.hex},result=${result.widen.unsignedBytes(32).hex})"
  }

  // for reasons I don't quite understand, embedding these implicitlys in object initializers directly,
  // lazy or not, led to errors in initialization ( null pointer or stack overflow). by trial and error,
  // this lazy computation in the parent object resolves the problem
  lazy val IntArrayClassTag  = implicitly[ClassTag[Array[Int]]];
  lazy val LongArrayClassTag = implicitly[ClassTag[Array[Long]]];

  final object Manager {

    final object Light                 extends Light( Implementation.Default );
    final object FullManualCaching     extends Full ( Implementation.Default );
    final object FullStochasticCaching extends Full ( Implementation.Default ) with StochasticNextCaching;

    val DoubleDag = Config.EthereumPowEthash23ManagerDoubleDag;

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

      override protected def buildDataset( epochNumber : Long, seed : Array[Byte], cache : implementation.Cache, fullSize : Long ) : implementation.Dataset = {
        val out = super.buildDataset( epochNumber, seed, cache, fullSize );
        midpersistEpochs.synchronized {
          val midpersist = midpersistEpochs( epochNumber );
          if (! midpersist ) {
            midpersistEpochs += epochNumber;
            Future[Unit] { 
              try { 
                implementation.cacheDataset( seed, out )
                  .warn( s"Failed to cache in-memory dataset for epoch number ${epochNumber} to ethash DAG file." ) 
              } finally {
                unmidpersistEpoch( epochNumber )
              }
            }
          }
        }
        out
      }
      private[this] def cacheEpoch( epochNumber : Long ) : Unit = {
        implementation.streamDagFileForEpochNumber( epochNumber ).warn(s"Failed to stream and cache ethash DAG file for epoch number ${epochNumber}.");
      }
    }

    abstract class Abstract( val implementation : Implementation ) extends Manager {
      protected final class EpochRecord( val epochNumber : Long, val seed : Array[Byte], val cache : implementation.Cache, val mbDataset : Option[implementation.Dataset] )

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
        persistedDataset.warn("Failed to load DAG file. Will create.").getOrElse {
          val fullSize = implementation.getFullSizeForEpoch( epochNumber );
          implementation.calcDataset( cache, fullSize )
        }
      }

      // extra arguments for use by mixins
      protected def buildDataset( epochNumber : Long, seed : Array[Byte], cache : implementation.Cache, fullSize : Long ) : implementation.Dataset = {
        implementation.calcDataset( cache, fullSize )
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
}
