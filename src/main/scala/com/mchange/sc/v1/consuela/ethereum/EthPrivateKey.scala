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

import specification.Types.{SignatureV, SignatureR, SignatureS}

import com.mchange.sc.v1.consuela.util.ByteSeqValue;

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import java.security.SecureRandom;

object EthPrivateKey {
  val ByteLength = crypto.secp256k1.ValueByteLength;

  def apply( bytes  : Seq[Byte] )    : EthPrivateKey = {
    bytes match {
      case iasb : ImmutableArraySeq.Byte => new EthPrivateKey( iasb );
      case other                         => new EthPrivateKey( ImmutableArraySeq.Byte( other.toArray ) ); //if from a mutable seq, we can't be sure the array is unshared, so we copy
    }
  }
  def apply( bytes  : Array[Byte] )  : EthPrivateKey = new EthPrivateKey( ImmutableArraySeq.Byte( bytes ) );
  def apply( bigInt : BigInt )       : EthPrivateKey = new EthPrivateKey( ImmutableArraySeq.Byte.createNoCopy( bigInt.unsignedBytes( ByteLength ) ) );
  def apply( random : SecureRandom ) : EthPrivateKey = {
    val bytes = Array.ofDim[Byte](ByteLength);
    random.nextBytes( bytes );
    new EthPrivateKey( ImmutableArraySeq.Byte.createNoCopy( bytes ) )
  }
}

final class EthPrivateKey private( val bytes : ImmutableArraySeq.Byte ) extends ByteSeqValue with ByteSeqValue.UnsignedBigIntegral {
  require( bytes.length == EthPrivateKey.ByteLength );

  def s = this.toBigInt;

  private def signRawBytes( rawBytes : Array[Byte] ) : EthSignature = { 
    import crypto.secp256k1._;
    signature( this.toBigInteger, rawBytes ) match {
      case Left( bytes ) => throw new UnexpectedSignatureFormatException( bytes.hex );
      case Right( Signature( r, s, Some( v ) ) ) => EthSignature( SignatureV( v ), SignatureR( BigInt(r) ), SignatureS( BigInt(s) ) );
      case Right( Signature( r, s, None ) )      => {
        val mbRecovered = recoverPublicKeyAndV( r, s, rawBytes );
        mbRecovered.fold( throw new EthereumException( s"Could find only partial signature [ v -> ???, r -> ${r}, s -> ${s} ]" ) ) { recovered =>
          EthSignature( SignatureV( recovered.v ), SignatureR( BigInt( r ) ), SignatureS( BigInt( s ) ) )
        }
      }
    }
  }
  def signEthHash( hash : EthHash ) = this.signRawBytes( hash.toByteArray );
  
  private def signEthHash( document : Array[Byte] ) : EthSignature = this.signEthHash( EthHash.hash( document ) );

  // default signing scheme
  def sign( document : Array[Byte] ) : EthSignature = this.signEthHash( document );

  lazy val toPublicKey = EthPublicKey( this );
}

