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

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import java.util.Arrays;

object EthPublicKey {
  val ByteLength = 2 * crypto.secp256k1.ValueByteLength;

  def apply( bytes : Seq[Byte] )     : EthPublicKey = new EthPublicKey( bytes.toArray );
  def apply( bytes : Array[Byte] )   : EthPublicKey = new EthPublicKey( bytes.clone() );
  def apply( priv  : EthPrivateKey ) : EthPublicKey = new EthPublicKey( this.computeBytes( priv ) );

  def computeBytes( priv : EthPrivateKey ) : Array[Byte] = crypto.secp256k1.computePublicKeyBytes( priv.toBigInteger )
}
final class EthPublicKey private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue {
  require( _bytes.length == EthPublicKey.ByteLength );

  private lazy val (_xBytes, _yBytes) = _bytes.splitAt( crypto.secp256k1.ValueByteLength );
  lazy val x = BigInt( new java.math.BigInteger( 1, _xBytes ) );
  lazy val y = BigInt( new java.math.BigInteger( 1, _yBytes ) );

  lazy val toAddress = EthAddress( this );

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


