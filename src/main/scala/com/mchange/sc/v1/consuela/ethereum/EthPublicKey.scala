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

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import java.util.Arrays;

import com.mchange.sc.v2.failable._;

object EthPublicKey {
  val ByteLength = 2 * crypto.secp256k1.ValueByteLength;

  def apply( bytes : Array[Byte], offset : Int ) : EthPublicKey = {
    val tmp = Array.ofDim[Byte]( ByteLength )
    Array.copy( bytes, offset, tmp, 0, ByteLength )
    new EthPublicKey( tmp );
  }
  def apply( bytes : Array[Byte] ) : EthPublicKey = EthPublicKey( bytes, 0 )
  def apply( bytes : Seq[Byte] ) : EthPublicKey = EthPublicKey( bytes.toArray );

  def apply( priv  : EthPrivateKey ) : EthPublicKey = new EthPublicKey( this.computeBytes( priv ) );

  def computeBytes( priv : EthPrivateKey ) : Array[Byte] = crypto.secp256k1.computePublicKeyBytes( priv.toBigInteger )

  def fromBytesWithUncompressedHeader( bytes : ByteSeqExact65 ) : Failable[EthPublicKey] = {
    val header = bytes.widen(0) 
    if ( header == 0x04 ) {
      succeed( apply( bytes.widen.toArray.drop(1) ) ) 
    } else {
      fail( s"Bad header for public key. We expressed the uncompressed marker, 0x04, found 0x${header.hex}" )
    }
  }
}
final class EthPublicKey private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue {
  import EthPublicKey.ByteLength

  require( _bytes.length == ByteLength );

  private lazy val (_xBytes, _yBytes) = _bytes.splitAt( crypto.secp256k1.ValueByteLength );
  lazy val x = BigInt( new java.math.BigInteger( 1, _xBytes ) );
  lazy val y = BigInt( new java.math.BigInteger( 1, _yBytes ) );

  lazy val toAddress = EthAddress( this );

  lazy val toByteSeqExact64 = ByteSeqExact64( this.bytes );

  lazy val bytesWithUncompressedHeader : ByteSeqExact65 = {
    val arr = Array.ofDim[Byte](ByteLength + 1)
    arr(0) = 0x04.toByte; // uncompressed header
    Array.copy( _bytes, 0, arr, 1, ByteLength )
    ByteSeqExact65( ImmutableArraySeq.Byte( arr ) )
  }

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


