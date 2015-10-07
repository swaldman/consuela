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

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import encoding.RLP;
import specification.Types._

import Implementation.Monitor

object JavaHelpers {
  private val ExpectedTruncatedBlockLength = EthBlock.Header.NumFields - 2;

  private def asHash( bytes : Array[Byte] ) : EthHash = EthHash.withBytes( bytes );
  private def asUnsigned256( bytes : Array[Byte] ) = Unsigned256( BigInt( 1, bytes ) );

  private val impl = Implementation.ParallelUInt32AsInt;
  private val managerFull  = new Manager.Full( impl ) with Manager.StochasticNextCaching; // won't hold or cache anything if untouched
  private val managerLight = new Manager.Light( impl ){}

  def buildHeader( 
    parentHash      : Array[Byte], 
    ommersHash      : Array[Byte], 
    coinbase        : Array[Byte], 
    stateRoot       : Array[Byte], 
    transactionRoot : Array[Byte], 
    receiptsRoot    : Array[Byte],
    logsBloom       : Array[Byte],
    difficulty      : Array[Byte],
    number          : Array[Byte],
    gasLimit        : Array[Byte],
    gasUsed         : Array[Byte],
    timestamp       : Array[Byte],
    extraData       : Array[Byte],
    mixHash         : Array[Byte],
    nonce           : Array[Byte]
  ) : EthBlock.Header = EthBlock.Header(
    asHash( parentHash ),
    asHash( ommersHash ),
    EthAddress( coinbase ),
    asHash( stateRoot ),
    asHash( transactionRoot ),
    asHash( receiptsRoot ),
    EthLogBloom.fromBytes( logsBloom ),
    asUnsigned256( difficulty ),
    asUnsigned256( number ),
    asUnsigned256( gasLimit ),
    asUnsigned256( gasUsed ),
    asUnsigned256( timestamp ),
    ByteSeqMax1024( extraData ),
    asHash( mixHash ),
    Unsigned64( BigInt( 1, nonce ) )
  )

  def headerFromTruncatedRLP( truncRLP : Array[Byte] ) : EthBlock.Header = {
    RLP.Element.decodeComplete( truncRLP ) match {
      case RLP.Element.Seq( seq ) => {
        if ( seq.length == ExpectedTruncatedBlockLength ) {
          RLP.fromElement[EthBlock.Header]( RLP.Element.Seq( seq :+ RLP.toElement( AllZeroesEthHash ) :+ RLP.toElement( Unsigned64(0) ) ) ).get
        } else {
          throw new IllegalArgumentException( s"A truncated EthBlock.Header should have s{ExpectedTruncatedBlockLength} fields, found ${seq.length}." );
        }
      }
      case _ => throw new IllegalArgumentException( s"truncRLP must be the RLP of a sequence. truncRLP -> ${truncRLP.hex}" );
    }
  }

  def streamDagFileForBlockNumber( blockNumber : Long, mf : Monitor.Factory ) : Boolean = {
    implicit val fact = if ( mf == null ) Monitor.Factory.NoOp else mf;

    impl.streamDagFileForBlockNumber( blockNumber ).isSucceeded;
  }
  def precomputeCacheDatasetForBlockNumber( blockNumber : Long, mf : Monitor.Factory ) : Boolean = {
    implicit val fact = if ( mf == null ) Monitor.Factory.NoOp else mf;

    impl.precomputeCacheDatasetForBlockNumber( blockNumber ).isSucceeded;
  }
  def getFullSizeForBlock( blockNumber : Long ) : Long = impl.getFullSizeForBlock( blockNumber );

  def noOpMonitorFactory : Implementation.Monitor.Factory = Monitor.Factory.NoOp;

  def jhashimoto( full : Boolean, truncHeaderRLP : Array[Byte], nonce : java.math.BigInteger ) : JHashimoto = {
    val truncHeader = headerFromTruncatedRLP( truncHeaderRLP );
    val hashimoto = (if (full) managerFull else managerLight).hashimoto( truncHeader, Unsigned64( nonce ) )
    new JHashimoto( hashimoto )
  }
  def jhashimotoFull( truncHeaderRLP : Array[Byte], nonce : java.math.BigInteger ) = jhashimoto( true, truncHeaderRLP, nonce );
  def jhashimotoLight( truncHeaderRLP : Array[Byte], nonce : java.math.BigInteger ) = jhashimoto( false, truncHeaderRLP, nonce );
}
final class JavaHelpers private () {} // just a placeholder for static forwarders

final class JHashimoto( val mixHash : Array[Byte], val result : java.math.BigInteger ) {
  def this( hashimoto : Hashimoto ) = this( hashimoto.mixDigest.toArray, hashimoto.result.widen.bigInteger )
}

