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

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v2.restrict._;

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import scala.collection._;

object Types {
  final object Min { // inclusive minima
    val SignatureV : Byte = 27;
  }
  final object Limit { // exclusive maxima
    val SignatureR : BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);
    val SignatureS : BigInt = { val TWO = BigInt(2); (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977) }
    val SignatureV : Byte   = 29;
  }

  /*
   * 
   * Companion objects representing restricted types
   * 
   */ 
  final object UnsignedBigInt extends RestrictedBigInt.Unsigned[UnsignedBigInt]                    { override protected def create( value : BigInt ) = new UnsignedBigInt( value ); }
  final object Unsigned8      extends RestrictedShort.UnsignedWithBitLength[Unsigned8]( 8 )        { override protected def create( value : Short  ) = new Unsigned8( value ); }
  final object Unsigned16     extends RestrictedInt.UnsignedWithBitLength[Unsigned16]( 16 )        { override protected def create( value : Int    ) = new Unsigned16( value ); }
  final object Unsigned64     extends RestrictedBigInt.UnsignedWithBitLength[Unsigned64]( 64 )     { override protected def create( value : BigInt ) = new Unsigned64( value ); }
  final object Unsigned256    extends RestrictedBigInt.UnsignedWithBitLength[Unsigned256]( 256 )   { override protected def create( value : BigInt ) = new Unsigned256( value ); }
  final object Unsigned2048   extends RestrictedBigInt.UnsignedWithBitLength[Unsigned2048]( 2048 ) { override protected def create( value : BigInt ) = new Unsigned2048( value ); }

  final object SignatureR    extends RestrictedBigInt.ZeroUntil[SignatureR]( Limit.SignatureR )              { override protected def create( value : BigInt ) = new SignatureR( value ); }
  final object SignatureS    extends RestrictedBigInt.ZeroUntil[SignatureS]( Limit.SignatureS )              { override protected def create( value : BigInt ) = new SignatureS( value ); }
  final object SignatureV    extends RestrictedByte.MinUntil[SignatureV]( Min.SignatureV, Limit.SignatureV ) { override protected def create( value : Byte   ) = new SignatureV( value ); }

  final object ByteSeqExact4   extends RestrictedByteSeq.ExactLength[ByteSeqExact4]( 4 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact4( value ); }
  final object ByteSeqExact8   extends RestrictedByteSeq.ExactLength[ByteSeqExact8]( 8 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact8( value ); }
  final object ByteSeqExact16  extends RestrictedByteSeq.ExactLength[ByteSeqExact16]( 16 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact16( value ); }
  final object ByteSeqExact20  extends RestrictedByteSeq.ExactLength[ByteSeqExact20]( 20 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact20( value ); }
  final object ByteSeqExact32  extends RestrictedByteSeq.ExactLength[ByteSeqExact32]( 32 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact32( value ); }
  final object ByteSeqExact64  extends RestrictedByteSeq.ExactLength[ByteSeqExact64]( 64 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact64( value ); }
  final object ByteSeqExact256 extends RestrictedByteSeq.ExactLength[ByteSeqExact256]( 256 )   { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact256( value ); }
  final object ByteSeqMax1024  extends RestrictedByteSeq.LimitedLength[ByteSeqMax1024]( 1024 ) { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqMax1024( value ); }

  final object StringUTF8 extends RestrictedString.NamedRestriction[StringUTF8]( StandardCharsets.UTF_8 ) { override protected def create( value : String ) = new StringUTF8( value ); }
  final object StringASCII_Exact3 extends RestrictedString.ExactLength[StringASCII_Exact3]( 3, StandardCharsets.US_ASCII ) 
  { override protected def create( value : String ) = new StringASCII_Exact3( value ); }

  /*
   * 
   * Value classes implementing types
   * 
   */ 
  final class UnsignedBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned8      private ( val widen : Short  ) extends AnyVal with RestrictedType.Element[Short];
  final class Unsigned16     private ( val widen : Int    ) extends AnyVal with RestrictedType.Element[Int];
  final class Unsigned64     private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned256    private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned2048   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  final class SignatureR   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class SignatureS   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class SignatureV   private ( val widen : Byte   ) extends AnyVal with RestrictedType.Element[Byte];

  final class ByteSeqExact4   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact8   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact16  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact20  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact32  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact64  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact256 private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqMax1024  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

  final class StringUTF8         private ( val widen : String ) extends AnyVal with RestrictedType.Element[String];
  final class StringASCII_Exact3 private ( val widen : String ) extends AnyVal with RestrictedType.Element[String];
}

