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

package com.mchange.sc.v1.consuela.ethereum.specification;

import scala.collection._;

import org.scalacheck._;
import com.mchange.sc.v2.restrict.scalacheck._;

object Arbitraries {

  private val ZERO = BigInt(0);

  private val BYTE_HIGH_BIT            : Int = 0x80;
  private val BYTE_HIGH_BIT_COMPLEMENT : Int = ~0x80;

  def genRandomByteArrayN( n : Int )        : Gen[Array[Byte]]         = Gen.containerOfN[Array,Byte]( n, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) );
  def genRandomImmutableByteSeqN( n : Int ) : Gen[immutable.Seq[Byte]] = Gen.containerOfN[immutable.Seq,Byte]( n, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) );

  def genRandomImmutableByteSeqMaxN( n : Int ) : Gen[immutable.Seq[Byte]] = Gen.choose(0, n).flatMap( genRandomImmutableByteSeqN( _ ) );

  private def genAlignedUnsignedBigInt( bitLength : Int ) : Gen[BigInt] = {
    require( bitLength % 8 == 0 );

    genRandomByteArrayN( bitLength / 8).map( BigInt(1, _ ) )
  }
  private def genArbitraryUnsignedBigInt( bitLength : Int ) : Gen[BigInt] = {
    val excessBits = (8 - (bitLength % 8) );

    def cropExcessBytes( bytes : Array[Byte] ) : Array[Byte] = { // we don't copy, we don't care about the original generated array, should we?
      var b : Int = bytes(0);
      (0 until excessBits).foreach { i => b &= (BYTE_HIGH_BIT_COMPLEMENT >> i ) }
      bytes(0) = b.toByte;
      bytes
    }

    genRandomByteArrayN( (bitLength + excessBits) / 8 ).map( cropExcessBytes ).map( BigInt(1, _ ) )
  }



  implicit val ArbitraryUnsignedBigInt  = Types.UnsignedBigInt.arbitrary;
  implicit val ArbitraryUnsigned8       = Types.Unsigned8.arbitrary;
  implicit val ArbitraryUnsigned64      = Types.Unsigned64.arbitraryFromGen( genAlignedUnsignedBigInt( 64 ) ); // to reduce the failure rate when producing values
  implicit val ArbitraryUnsigned256     = Types.Unsigned256.arbitraryFromGen( genAlignedUnsignedBigInt( 256 ) );
  implicit val ArbitraryUnsigned2048    = Types.Unsigned2048.arbitrary;

  implicit val ArbitrarySignatureR      = Types.SignatureR.arbitraryFromGen( genArbitraryUnsignedBigInt( (Types.Limit.SignatureR - 1).bitLength ) );
  implicit val ArbitrarySignatureS      = Types.SignatureS.arbitraryFromGen( genArbitraryUnsignedBigInt( (Types.Limit.SignatureS - 1).bitLength ) );
  implicit val ArbitrarySignatureV      = Types.SignatureV.arbitraryFromGen( Gen.choose(Types.Min.SignatureV, (Types.Limit.SignatureV - 1).toByte) ); //choose's range in inclusive, Limit is exclusive

  implicit val ArbitraryByteSeqExact4   = Types.ByteSeqExact4.arbitraryFromGen( genRandomImmutableByteSeqN( 4 ) );
  implicit val ArbitraryByteSeqExact8   = Types.ByteSeqExact8.arbitraryFromGen( genRandomImmutableByteSeqN( 8 ) );
  implicit val ArbitraryByteSeqExact20  = Types.ByteSeqExact20.arbitraryFromGen( genRandomImmutableByteSeqN( 20 ) );
  implicit val ArbitraryByteSeqExact32  = Types.ByteSeqExact32.arbitraryFromGen( genRandomImmutableByteSeqN( 32 ) );
  implicit val ArbitraryByteSeqExact256 = Types.ByteSeqExact256.arbitraryFromGen( genRandomImmutableByteSeqN( 256 ) );

  implicit val ArbitraryByteSeqMax1024  = Types.ByteSeqMax1024.arbitraryFromGen( genRandomImmutableByteSeqMaxN( 1024 ) );
}
