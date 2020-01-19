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

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import com.mchange.sc.v3.failable._;

import java.util.Arrays;

import scala.collection._;

object EthPublicKey {
  def apply( priv  : EthPrivateKey ) : EthPublicKey = new EthPublicKey( ByteSeqExact64( this.computeBytes( priv ) ) );

  def computeBytes( priv : EthPrivateKey ) : Array[Byte] = crypto.secp256k1.computePublicKeyBytes( priv.s.bigInteger )

  def fromBytesWithUncompressedHeader( bytes : ByteSeqExact65 ) : Failable[EthPublicKey] = {
    val header = bytes.widen(0) 
    if ( header == 0x04 ) {
      Failable.succeed( apply( ByteSeqExact64.assert( bytes.widen.toArray.drop(1) ) ) ) 
    } else {
      Failable.fail( s"Bad header for public key. We expressed the uncompressed marker, 0x04, found 0x${header.hex}" )
    }
  }
}
final case class EthPublicKey( val bytes : ByteSeqExact64 ) {

  private lazy val _arr : Array[Byte] = bytes.widen.toArray

  private lazy val (_xBytes, _yBytes) = _arr.splitAt( crypto.secp256k1.ValueByteLength );

  lazy val x = BigInt( new java.math.BigInteger( 1, _xBytes ) );
  lazy val y = BigInt( new java.math.BigInteger( 1, _yBytes ) );

  lazy val toAddress = EthAddress( this )

  def address = this.toAddress

  lazy val bytesWithUncompressedHeader : ByteSeqExact65 = {
    val buff = new mutable.ArrayBuffer[Byte](65)
    buff += 0x04.toByte; // uncompressed header
    buff ++= bytes.widen
    ByteSeqExact65( ImmutableArraySeq.Byte( buff.toArray ) )
  }

  def hex = bytes.widen.hex

  def hex0x = bytes.widen.hex0x

  def matches( priv : EthPrivateKey ) : Boolean = Arrays.equals( _arr, EthPublicKey.computeBytes( priv ) );

  // default scheme
  def verify( document : Array[Byte], signature : EthSignature.Basic ) : Boolean = {
    def verifyRawBytes( rawBytes : Array[Byte], signature : EthSignature.Basic ) : Boolean = {
      val _signature = crypto.secp256k1.Signature( signature.r.widen.bigInteger, signature.s.widen.bigInteger )
      crypto.secp256k1.verifySignature( rawBytes, _signature, this.x.bigInteger, this.y.bigInteger )
    }
    def verifyEthHash( hash : EthHash, signature : EthSignature.Basic ) : Boolean = {
      verifyRawBytes( hash.toByteArray, signature );
    }
    def verifyHashedDocument( document : Array[Byte], signature : EthSignature.Basic ) : Boolean = {
      verifyEthHash( EthHash.hash( document ), signature );
    }

    verifyHashedDocument( document, signature )
  }
}


