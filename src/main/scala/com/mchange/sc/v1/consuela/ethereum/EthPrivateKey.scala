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

import specification.Types.{ByteSeqExact32, SignatureV, SignatureR, SignatureS}

import com.mchange.sc.v1.consuela.util.ByteSeqValue;

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import java.security.SecureRandom;

object EthPrivateKey {
  val ByteLength = 32 // crypto.secp256k1.ValueByteLength

  def apply( bigInt : BigInt ) : EthPrivateKey = {
    val bytes = ByteSeqExact32( ImmutableArraySeq.Byte.createNoCopy( bigInt.unsignedBytes( ByteLength ) ) )
    EthPrivateKey( bytes );
  }
  def apply( random : SecureRandom ) : EthPrivateKey = {
    val bytes = ByteSeqExact32( ImmutableArraySeq.Byte.random( ByteLength )( random ) )
    EthPrivateKey( bytes )
  }
  def apply( hexString : String ) : EthPrivateKey = {
    EthPrivateKey( ByteSeqExact32( hexString.decodeHex ) )
  }
}

final case class EthPrivateKey( val bytes : ByteSeqExact32 ) {

  lazy val s = bytes.widen.toUnsignedBigInt

  // default signing scheme
  def sign( document : Array[Byte] ) : EthSignature = signHashedDocument( document );

  def signRawBytes( rawBytes : Array[Byte] ) : EthSignature = {
    import crypto.secp256k1._;
    recoverableCompleteSignature( s.bigInteger, rawBytes ) match {
      case Left( badbytes )                              => throw new UnexpectedSignatureFormatException( badbytes.hex );
      case Right( Signature( r, s, Some( v ) ) )         => EthSignature( SignatureV( v ), SignatureR( BigInt(r) ), SignatureS( BigInt(s) ) );
      case Right( incomplete @ Signature( r, s, None ) ) => throw new UnexpectedSignatureFormatException( s"We expect a complete signature. Missing v: ${incomplete}" );
    }
  }
  def signEthHash( hash : EthHash ) = signRawBytes( hash.toByteArray );
  def signHashedDocument( document : Array[Byte] ) : EthSignature = signEthHash( EthHash.hash( document ) );

  lazy val toPublicKey = EthPublicKey( this );
}

