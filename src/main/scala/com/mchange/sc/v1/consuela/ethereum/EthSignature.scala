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

import specification.Types.{ByteSeqExact64, ByteSeqExact65, SignatureV, SignatureR, SignatureS, SignatureRecId, SignatureWithChainIdV, UnsignedBigInt};

// what should I name 32 so that I don't keep retyping 32?
object EthSignature {
  private def fromBytesVRS( arr : Array[Byte], offset : Int, vAsRecId : Boolean ) : EthSignature = {
    val v = {
      val b = arr( offset )
      SignatureV( if ( vAsRecId ) crypto.secp256k1.vFromRecId(b) else b )
    }
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
  private def fromBytesRSV( arr : Array[Byte], offset : Int, vAsRecId : Boolean ) : EthSignature = {
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
    val v = {
      val b = arr( offset+64 )
      SignatureV( if ( vAsRecId ) crypto.secp256k1.vFromRecId(b) else b )
    }
    EthSignature( v, r, s )
  }
  def fromBytesVRS( arr : Array[Byte], offset : Int ) : EthSignature = fromBytesVRS( arr, offset, false )
  def fromBytesVRS( arr : Array[Byte] ) : EthSignature = fromBytesVRS( arr, 0 )
  def fromBytesVRS( seq : Seq[Byte] )   : EthSignature = fromBytesVRS( seq.toArray )
  def fromBytesRSV( arr : Array[Byte], offset : Int ) : EthSignature = fromBytesRSV( arr, offset, false )
  def fromBytesRSV( arr : Array[Byte] ) : EthSignature = fromBytesRSV( arr, 0 )
  def fromBytesRSV( seq : Seq[Byte] )   : EthSignature = fromBytesRSV( seq.toArray )
  def fromBytesIRS( arr : Array[Byte], offset : Int ) : EthSignature = fromBytesVRS( arr, offset, true )
  def fromBytesIRS( arr : Array[Byte] ) : EthSignature = fromBytesIRS( arr, 0 )
  def fromBytesIRS( seq : Seq[Byte] )   : EthSignature = fromBytesIRS( seq.toArray )
  def fromBytesRSI( arr : Array[Byte], offset : Int ) : EthSignature = fromBytesRSV( arr, offset, true )
  def fromBytesRSI( arr : Array[Byte] ) : EthSignature = fromBytesRSI( arr, 0 )
  def fromBytesRSI( seq : Seq[Byte] )   : EthSignature = fromBytesRSI( seq.toArray )

  object Base {
    def apply( v : UnsignedBigInt, r : SignatureR, s : SignatureS ) : Base = {
      if ( v.widen.isValidByte && SignatureV.contains( v.widen.toByte ) ) {
        EthSignature( SignatureV( v.widen.toByte ), r, s )
      }
      else if (SignatureWithChainIdV.contains(v.widen)) {
        EthSignature.WithChainId( SignatureWithChainIdV(v.widen), r, s )
      }
      else {
        throw new IllegalArgumentException( s"${v.widen} is not a valid value for a signature v, with or without an encode EIP-155 Chain ID." )
      }
    }
  }
  sealed trait Base {
    def r : SignatureR
    def s : SignatureS

    def untypedV : UnsignedBigInt

    def wasSigned( document : Array[Byte] ) : Option[EthPublicKey]

    def signsForAddress( document : Array[Byte], address : EthAddress ) : Boolean = wasSigned( document ).map( _.toAddress ).fold( false )( _ == address )
  }

  /**
    * Represents an EIP 155 signature with embedded Chain ID
    * 
    * See https://eips.ethereum.org/EIPS/eip-155
    */
  object WithChainId {
    def apply( simple : EthSignature, chainId : EthChainId ) : WithChainId = {
      val v = chainId.signatureWithChainIdV( simple.v )
      val r = simple.r
      val s = simple.s
      apply( v, r, s )
    }
  }
  final case class WithChainId( val v : SignatureWithChainIdV, val r : SignatureR, val s : SignatureS ) extends Base {
    private lazy val extracted = EthChainId.extract( v )
    def ethSignature : EthSignature = EthSignature( extracted._1, r, s ) 
    def chainId      : EthChainId   = extracted._2

    def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethSignature.wasSigned( document )

    def wasSigned( document : Array[Byte], forChainId : EthChainId ) : Option[EthPublicKey] = if ( forChainId == chainId ) wasSigned( document ) else None

    def signsForAddress( document : Array[Byte], address : EthAddress, forChainId : EthChainId ) : Boolean = forChainId == chainId && signsForAddress( document, address )

    lazy val untypedV : UnsignedBigInt = UnsignedBigInt(v.widen)
  }
}

final case class EthSignature( val v : SignatureV, val r : SignatureR, val s : SignatureS ) extends EthSignature.Base {

  private def rawBytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.recoverPublicKeyBytesV( v.widen, r.widen.bigInteger, s.widen.bigInteger, bytes ).map( bytes => EthPublicKey( ByteSeqExact64( bytes ) ) )
  }

  def ethHashWasSigned( hash : EthHash )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray );
  private def ethHashOfDocumentWasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) );

  // default
  def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = this.ethHashOfDocumentWasSigned( document );

  lazy val untypedV : UnsignedBigInt = UnsignedBigInt(v.widen.toInt) // XXX: promotes Byte to Int without truncation. fix restrict library to handle this automatically, as it claims it should

  lazy val exportBytesVRS : ByteSeqExact65 = ByteSeqExact65.assert( v.widen +: Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) : _* ) );
  lazy val exportBytesRSV : ByteSeqExact65 = ByteSeqExact65.assert( Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) :+ v.widen : _* ) );
  lazy val exportBytesIRS : ByteSeqExact65 = ByteSeqExact65.assert( recId.widen +: Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) : _* ) );
  lazy val exportBytesRSI : ByteSeqExact65 = ByteSeqExact65.assert( Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) :+ recId.widen : _* ) );

  val recId : SignatureRecId = SignatureRecId( crypto.secp256k1.recIdFromV( v.widen ) )

  val isHomesteadCompatible = (s.widen < Homestead.LimitSignatureS)
}
