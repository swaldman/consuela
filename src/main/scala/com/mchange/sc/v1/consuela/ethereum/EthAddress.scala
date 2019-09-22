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

package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import encoding.{RLP,Nibble}

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact20, Unsigned256}

import scala.collection._

object EthAddress {
  val ByteLength = 20;

  def apply( pub : EthPublicKey ) : EthAddress = new EthAddress( this.computeBytes( pub ) );

  def computeBytes( pub : EthPublicKey ) : ByteSeqExact20 = ByteSeqExact20( EthHash.hash(pub.bytes.widen).toByteArray.drop(12) );

  def apply( hexString : String ) : EthAddress = EthAddress( ByteSeqExact20( hexString.decodeHex ) )

  def apply( bytes : Array[Byte] ) : EthAddress = EthAddress( ByteSeqExact20( bytes ) )

  def apply( bytes : Seq[Byte] ) : EthAddress = EthAddress( ByteSeqExact20( bytes ) )

  def forContract( creator : EthAddress, nonce : Unsigned256 ) : EthAddress = {  // see https://ethereum.stackexchange.com/questions/760/how-is-the-address-of-an-ethereum-contract-computed
    val seq = RLP.Element.Seq.of( RLP.toElement(creator), RLP.toElement(nonce) )
    val seqBytes = RLP.Element.encode( seq )
    val hashBytes = EthHash.hash( seqBytes ).bytes
    EthAddress( hashBytes.drop(12) )
  }

  val Zero = EthAddress( ByteSeqExact20( Array.fill[Byte](20)(0.toByte) ) )

  trait Source[-T] {
    /**
      * May throw if the instance of T cannot be converted
      */ 
    def toEthAddress( t : T ) : EthAddress
  }
  implicit final object EthAddressIsSource extends EthAddress.Source[EthAddress]{
    def toEthAddress( address : EthAddress ) = address
  }
  implicit final object StringIsSource extends EthAddress.Source[String]{
    def toEthAddress( hex : String ) = EthAddress( hex )
  }
  implicit final object ByteSeqIsSource extends EthAddress.Source[Seq[Byte]]{
    def toEthAddress( seq : Seq[Byte] ) = EthAddress( seq )
  }
  implicit final object ImmutableByteSeqIsSource extends EthAddress.Source[immutable.Seq[Byte]]{
    def toEthAddress( seq : immutable.Seq[Byte] ) = EthAddress( seq )
  }
  implicit final object ByteArrayIsSource extends EthAddress.Source[Array[Byte]]{
    def toEthAddress( arr : Array[Byte] ) = EthAddress( arr )
  }
  implicit final object ByteSeqExact20IsSource extends EthAddress.Source[ByteSeqExact20]{
    def toEthAddress( bytes : ByteSeqExact20 ) = EthAddress( bytes )
  }
}
final case class EthAddress( val bytes : ByteSeqExact20 ) {
  lazy val toNibbles : IndexedSeq[Nibble] = encoding.toNibbles( bytes.widen )

  def hex = bytes.widen.hex

  def matches( pub : EthPublicKey ) : Boolean = bytes == EthAddress.computeBytes( pub );
}
