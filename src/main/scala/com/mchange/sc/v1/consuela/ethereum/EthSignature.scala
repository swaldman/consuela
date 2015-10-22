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

import scala.collection._;

import specification.Types.{ByteSeqExact65, SignatureV, SignatureR, SignatureS, SignatureRecId};

// what should I name 32 so that I don't keep retyping 32?
object EthSignature {
  def fromBytesVRS( arr : Array[Byte], offset : Int ) : EthSignature = {
    val v = SignatureV( arr( offset ) )
    val r = {
      val tmp = Array.ofDim[Byte](32)
      Array.copy( arr, offset+1, tmp, 0, 32 )
      SignatureR( BigInt(1, tmp) )
    }
    val s = {
      val tmp = Array.ofDim[Byte](32);
      Array.copy( arr, offset+33, tmp, 0, 32 );
      SignatureS( BigInt(1, tmp) )
    }
    EthSignature( v, r, s )
  }
  def fromBytesVRS( arr : Array[Byte] ) : EthSignature = fromBytesVRS( arr, 0 )
  def fromBytesVRS( seq : Seq[Byte] )   : EthSignature = fromBytesVRS( seq.toArray )
  def fromBytesRSV( arr : Array[Byte], offset : Int ) : EthSignature = {
    val r = {
      val tmp = Array.ofDim[Byte](32)
      Array.copy( arr, offset, tmp, 0, 32 )
      SignatureR( BigInt(1, tmp) )
    }
    val s = {
      val tmp = Array.ofDim[Byte](32);
      Array.copy( arr, offset+32, tmp, 0, 32 );
      SignatureS( BigInt(1, tmp) )
    }
    val v = SignatureV( arr( offset+64 ) )
    EthSignature( v, r, s )
  }
  def fromBytesRSV( arr : Array[Byte] ) : EthSignature = fromBytesRSV( arr, 0 )
  def fromBytesRSV( seq : Seq[Byte] )   : EthSignature = fromBytesRSV( seq.toArray )
}

final case class EthSignature( val v : SignatureV, val r : SignatureR, val s : SignatureS ) {

  /*
  // this behaves oddly. hmm. But EthPublickKey verify works fine.

  private def rawBytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.recoverPublicKeyBytesV( v.widen, r.widen.bigInteger, s.widen.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  private def ethHashWasSigned( hash : EthHash )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray );
  private def ethHashWasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) );

  // default
  def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = this.ethHashWasSigned( document );
  */ 

  lazy val exportBytesVRS : ByteSeqExact65 = ByteSeqExact65.assert( v.widen +: Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) : _* ) );
  lazy val exportBytesRSV : ByteSeqExact65 = ByteSeqExact65.assert( Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) :+ v.widen : _* ) );

  val recId : SignatureRecId = SignatureRecId( crypto.secp256k1.recIdFromV( v.widen ) )
}
