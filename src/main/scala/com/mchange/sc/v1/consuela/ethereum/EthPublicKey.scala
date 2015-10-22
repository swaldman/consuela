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
import com.mchange.sc.v1.consuela.crypto;

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact64,ByteSeqExact65}

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import java.util.Arrays;

object EthPublicKey {
  val ByteLength = 2 * crypto.secp256k1.ValueByteLength;
  val ExportByteLength = ByteLength + 1

  val UncompressedMarker = 0x04.toByte

  def apply( bytes : Array[Byte], offset : Int, len : Int ) : EthPublicKey = {
    val realOffset = len match {
      case ByteLength       => offset
      case ExportByteLength => {
        require( bytes(offset) == UncompressedMarker, s"EthPublicKeys may be represented as ${ExportByteLength}-byte sequences only of the first byte is uncompressed marker 0x04" )
        offset+1
      }
      case _ => throw new IllegalArgumentException( s"Only sequences of ${ByteLength} or ${ExportByteLength} bytes can be interpreted as EthPublicKeys. [len: ${len}]" )
    }
    val tmp = Array.ofDim[Byte]( ByteLength )
    Array.copy( bytes, realOffset, tmp, 0, ByteLength )
    new EthPublicKey( tmp );
  }
  def apply( bytes : Array[Byte] ) : EthPublicKey = EthPublicKey( bytes, 0, bytes.length )
  def apply( bytes : Seq[Byte] ) : EthPublicKey = EthPublicKey( bytes.toArray );

  def apply( priv  : EthPrivateKey ) : EthPublicKey = new EthPublicKey( this.computeBytes( priv ) );

  def computeBytes( priv : EthPrivateKey ) : Array[Byte] = crypto.secp256k1.computePublicKeyBytes( priv.toBigInteger )
}
final class EthPublicKey private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue {
  require( _bytes.length == EthPublicKey.ByteLength );

  private lazy val (_xBytes, _yBytes) = _bytes.splitAt( crypto.secp256k1.ValueByteLength );
  lazy val x = BigInt( new java.math.BigInteger( 1, _xBytes ) );
  lazy val y = BigInt( new java.math.BigInteger( 1, _yBytes ) );

  lazy val toAddress = EthAddress( this );

  lazy val toByteSeqExact64 = ByteSeqExact64( this.bytes );

  lazy val exportWithUncompressedMarker = ByteSeqExact65( EthPublicKey.UncompressedMarker +: toByteSeqExact64.widen ); // with ECIES uncompressed marker, 0x04

  def matches( priv : EthPrivateKey ) : Boolean = Arrays.equals( _bytes, EthPublicKey.computeBytes( priv ) );

  private def verifyRawBytes( rawBytes : Array[Byte], signature : EthSignature ) : Boolean = {
    val _signature = crypto.secp256k1.Signature( signature.r.widen.bigInteger, signature.s.widen.bigInteger )
    crypto.secp256k1.verifySignature( rawBytes, _signature, this.x.bigInteger, this.y.bigInteger )
  }
  private def verifyEthHash( hash : EthHash, signature : EthSignature ) : Boolean = this.verifyRawBytes( hash.toByteArray, signature );

  private def verifyEthHash( document : Array[Byte], signature : EthSignature ) : Boolean = {
    this.verifyEthHash( EthHash.hash( document ), signature );
  }

  // default scheme
  def verify( document : Array[Byte], signature : EthSignature ) : Boolean = this.verifyEthHash( document, signature );
}


