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
    val SignatureR : BigInt   = BigInt(1)
    val SignatureS : BigInt   = Min.SignatureR
    val SignatureV : Byte     = 27
    val SignatureWithChainIdV = 37 // see EIP 155, https://eips.ethereum.org/EIPS/eip-155
  }

  // following Ethereum Homestead (Block 1,150,000), the maximum value of SignatureS should be SignatureR/2
  // but we leave that out of its general type, so that older signatures remain readable
  final object Limit { // exclusive maxima
    val SignatureR     : BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);
    val SignatureS     : BigInt = { val TWO = BigInt(2); (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977) }
    val SignatureV     : Byte   = 29; // we expect only vals 27 or 28
    val SignatureRecId : Byte   =  2; // we expect only vals 0 or 1
  }

  /*
   * 
   * Companion objects representing restricted types
   * 
   */


  // Note: For now, signed integral restrictions, just used for solidity stubs, are NOT RLP serializable.
  val Int8  = RestrictedByte.AnyByte
  val Int16 = RestrictedShort.AnyShort

  final object Int24 extends RestrictedInt.TwosComplementWithBitLength[Int24]( 24 ) { override protected def create( value : Int ) = new Int24( value ); }

  val Int32 = RestrictedInt.AnyInt

  final object Int40  extends RestrictedBigInt.TwosComplementWithBitLength[Int40] ( 40  ) { override protected def create( value : BigInt ) = new Int40 ( value ); }
  final object Int48  extends RestrictedBigInt.TwosComplementWithBitLength[Int48] ( 48  ) { override protected def create( value : BigInt ) = new Int48 ( value ); }
  final object Int56  extends RestrictedBigInt.TwosComplementWithBitLength[Int56] ( 56  ) { override protected def create( value : BigInt ) = new Int56 ( value ); }
  final object Int64  extends RestrictedBigInt.TwosComplementWithBitLength[Int64] ( 64  ) { override protected def create( value : BigInt ) = new Int64 ( value ); }
  final object Int72  extends RestrictedBigInt.TwosComplementWithBitLength[Int72] ( 72  ) { override protected def create( value : BigInt ) = new Int72 ( value ); }
  final object Int80  extends RestrictedBigInt.TwosComplementWithBitLength[Int80] ( 80  ) { override protected def create( value : BigInt ) = new Int80 ( value ); }
  final object Int88  extends RestrictedBigInt.TwosComplementWithBitLength[Int88] ( 88  ) { override protected def create( value : BigInt ) = new Int88 ( value ); }
  final object Int96  extends RestrictedBigInt.TwosComplementWithBitLength[Int96] ( 96  ) { override protected def create( value : BigInt ) = new Int96 ( value ); }
  final object Int104 extends RestrictedBigInt.TwosComplementWithBitLength[Int104]( 104 ) { override protected def create( value : BigInt ) = new Int104( value ); }
  final object Int112 extends RestrictedBigInt.TwosComplementWithBitLength[Int112]( 112 ) { override protected def create( value : BigInt ) = new Int112( value ); }
  final object Int120 extends RestrictedBigInt.TwosComplementWithBitLength[Int120]( 120 ) { override protected def create( value : BigInt ) = new Int120( value ); }
  final object Int128 extends RestrictedBigInt.TwosComplementWithBitLength[Int128]( 128 ) { override protected def create( value : BigInt ) = new Int128( value ); }
  final object Int136 extends RestrictedBigInt.TwosComplementWithBitLength[Int136]( 136 ) { override protected def create( value : BigInt ) = new Int136( value ); }
  final object Int144 extends RestrictedBigInt.TwosComplementWithBitLength[Int144]( 144 ) { override protected def create( value : BigInt ) = new Int144( value ); }
  final object Int152 extends RestrictedBigInt.TwosComplementWithBitLength[Int152]( 152 ) { override protected def create( value : BigInt ) = new Int152( value ); }
  final object Int160 extends RestrictedBigInt.TwosComplementWithBitLength[Int160]( 160 ) { override protected def create( value : BigInt ) = new Int160( value ); }
  final object Int168 extends RestrictedBigInt.TwosComplementWithBitLength[Int168]( 168 ) { override protected def create( value : BigInt ) = new Int168( value ); }
  final object Int176 extends RestrictedBigInt.TwosComplementWithBitLength[Int176]( 176 ) { override protected def create( value : BigInt ) = new Int176( value ); }
  final object Int184 extends RestrictedBigInt.TwosComplementWithBitLength[Int184]( 184 ) { override protected def create( value : BigInt ) = new Int184( value ); }
  final object Int192 extends RestrictedBigInt.TwosComplementWithBitLength[Int192]( 192 ) { override protected def create( value : BigInt ) = new Int192( value ); }
  final object Int200 extends RestrictedBigInt.TwosComplementWithBitLength[Int200]( 200 ) { override protected def create( value : BigInt ) = new Int200( value ); }
  final object Int208 extends RestrictedBigInt.TwosComplementWithBitLength[Int208]( 208 ) { override protected def create( value : BigInt ) = new Int208( value ); }
  final object Int216 extends RestrictedBigInt.TwosComplementWithBitLength[Int216]( 216 ) { override protected def create( value : BigInt ) = new Int216( value ); }
  final object Int224 extends RestrictedBigInt.TwosComplementWithBitLength[Int224]( 224 ) { override protected def create( value : BigInt ) = new Int224( value ); }
  final object Int232 extends RestrictedBigInt.TwosComplementWithBitLength[Int232]( 232 ) { override protected def create( value : BigInt ) = new Int232( value ); }
  final object Int240 extends RestrictedBigInt.TwosComplementWithBitLength[Int240]( 240 ) { override protected def create( value : BigInt ) = new Int240( value ); }
  final object Int248 extends RestrictedBigInt.TwosComplementWithBitLength[Int248]( 248 ) { override protected def create( value : BigInt ) = new Int248( value ); }
  final object Int256 extends RestrictedBigInt.TwosComplementWithBitLength[Int256]( 256 ) { override protected def create( value : BigInt ) = new Int256( value ); }
  
  final object UnsignedBigInt extends RestrictedBigInt.Unsigned[UnsignedBigInt] { override protected def create( value : BigInt ) = new UnsignedBigInt( value ); }

  final object Unsigned1   extends RestrictedByte.UnsignedWithBitLength[Unsigned1]( 1 )         { override protected def create( value : Byte  )  = new Unsigned1( value ); }
  final object Unsigned8   extends RestrictedShort.UnsignedWithBitLength[Unsigned8]( 8 )        { override protected def create( value : Short  ) = new Unsigned8( value ); }
  final object Unsigned16  extends RestrictedInt.UnsignedWithBitLength[Unsigned16]( 16 )        { override protected def create( value : Int    ) = new Unsigned16( value ); }
  final object Unsigned24  extends RestrictedInt.UnsignedWithBitLength[Unsigned24]( 24 )        { override protected def create( value : Int    ) = new Unsigned24( value ); }
  final object Unsigned32  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned32]( 32 )     { override protected def create( value : BigInt ) = new Unsigned32( value ); }
  final object Unsigned40  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned40]( 40 )     { override protected def create( value : BigInt ) = new Unsigned40( value ); }
  final object Unsigned48  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned48]( 48 )     { override protected def create( value : BigInt ) = new Unsigned48( value ); }
  final object Unsigned56  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned56]( 56 )     { override protected def create( value : BigInt ) = new Unsigned56( value ); }
  final object Unsigned64  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned64]( 64 )     { override protected def create( value : BigInt ) = new Unsigned64( value ); }
  final object Unsigned72  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned72]( 72 )     { override protected def create( value : BigInt ) = new Unsigned72( value ); }
  final object Unsigned80  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned80]( 80 )     { override protected def create( value : BigInt ) = new Unsigned80( value ); }
  final object Unsigned88  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned88]( 88 )     { override protected def create( value : BigInt ) = new Unsigned88( value ); }
  final object Unsigned96  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned96]( 96 )     { override protected def create( value : BigInt ) = new Unsigned96( value ); }
  final object Unsigned104 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned104]( 104 )   { override protected def create( value : BigInt ) = new Unsigned104( value ); }
  final object Unsigned112 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned112]( 112 )   { override protected def create( value : BigInt ) = new Unsigned112( value ); }
  final object Unsigned120 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned120]( 120 )   { override protected def create( value : BigInt ) = new Unsigned120( value ); }
  final object Unsigned128 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned128]( 128 )   { override protected def create( value : BigInt ) = new Unsigned128( value ); }
  final object Unsigned136 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned136]( 136 )   { override protected def create( value : BigInt ) = new Unsigned136( value ); }
  final object Unsigned144 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned144]( 144 )   { override protected def create( value : BigInt ) = new Unsigned144( value ); }
  final object Unsigned152 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned152]( 152 )   { override protected def create( value : BigInt ) = new Unsigned152( value ); }
  final object Unsigned160 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned160]( 160 )   { override protected def create( value : BigInt ) = new Unsigned160( value ); }
  final object Unsigned168 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned168]( 168 )   { override protected def create( value : BigInt ) = new Unsigned168( value ); }
  final object Unsigned176 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned176]( 176 )   { override protected def create( value : BigInt ) = new Unsigned176( value ); }
  final object Unsigned184 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned184]( 184 )   { override protected def create( value : BigInt ) = new Unsigned184( value ); }
  final object Unsigned192 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned192]( 192 )   { override protected def create( value : BigInt ) = new Unsigned192( value ); }
  final object Unsigned200 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned200]( 200 )   { override protected def create( value : BigInt ) = new Unsigned200( value ); }
  final object Unsigned208 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned208]( 208 )   { override protected def create( value : BigInt ) = new Unsigned208( value ); }
  final object Unsigned216 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned216]( 216 )   { override protected def create( value : BigInt ) = new Unsigned216( value ); }
  final object Unsigned224 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned224]( 224 )   { override protected def create( value : BigInt ) = new Unsigned224( value ); }
  final object Unsigned232 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned232]( 232 )   { override protected def create( value : BigInt ) = new Unsigned232( value ); }
  final object Unsigned240 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned240]( 240 )   { override protected def create( value : BigInt ) = new Unsigned240( value ); }
  final object Unsigned248 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned248]( 248 )   { override protected def create( value : BigInt ) = new Unsigned248( value ); }
  final object Unsigned256 extends RestrictedBigInt.UnsignedWithBitLength[Unsigned256]( 256 )   { override protected def create( value : BigInt ) = new Unsigned256( value ); }

  final object Unsigned2048   extends RestrictedBigInt.UnsignedWithBitLength[Unsigned2048]( 2048 ) { override protected def create( value : BigInt ) = new Unsigned2048( value ); }

  final object SignatureR            extends RestrictedBigInt.MinUntil[SignatureR]( Min.SignatureR, Limit.SignatureR )               { override protected def create( value : BigInt ) = new SignatureR( value ); }
  final object SignatureS            extends RestrictedBigInt.MinUntil[SignatureS]( Min.SignatureS, Limit.SignatureS )               { override protected def create( value : BigInt ) = new SignatureS( value ); }
  final object SignatureV            extends RestrictedByte.MinUntil[SignatureV]( Min.SignatureV, Limit.SignatureV )                 { override protected def create( value : Byte   ) = new SignatureV( value ); }
  final object SignatureRecId        extends RestrictedByte.ZeroUntil[SignatureRecId]( Limit.SignatureRecId )                        { override protected def create( value : Byte   ) = new SignatureRecId( value ); }
  final object SignatureWithChainIdV extends RestrictedInt.MinUntil[SignatureWithChainIdV]( Min.SignatureWithChainIdV, Int.MaxValue) { override protected def create( value : Int   )  = new SignatureWithChainIdV( value ); }

  // for solidity stubs, we need the full set of type restrictions from bytes1 to bytes32. grrr.
  // codegeneration in the REPL to the rescue...

  final object ByteSeqExact1    extends RestrictedByteSeq.ExactLength[ByteSeqExact1]( 1 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact1( value ); }
  final object ByteSeqExact2    extends RestrictedByteSeq.ExactLength[ByteSeqExact2]( 2 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact2( value ); }
  final object ByteSeqExact3    extends RestrictedByteSeq.ExactLength[ByteSeqExact3]( 3 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact3( value ); }
  final object ByteSeqExact4    extends RestrictedByteSeq.ExactLength[ByteSeqExact4]( 4 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact4( value ); }
  final object ByteSeqExact5    extends RestrictedByteSeq.ExactLength[ByteSeqExact5]( 5 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact5( value ); }
  final object ByteSeqExact6    extends RestrictedByteSeq.ExactLength[ByteSeqExact6]( 6 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact6( value ); }
  final object ByteSeqExact7    extends RestrictedByteSeq.ExactLength[ByteSeqExact7]( 7 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact7( value ); }
  final object ByteSeqExact8    extends RestrictedByteSeq.ExactLength[ByteSeqExact8]( 8 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact8( value ); }
  final object ByteSeqExact9    extends RestrictedByteSeq.ExactLength[ByteSeqExact9]( 9 )         { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact9( value ); }
  final object ByteSeqExact10   extends RestrictedByteSeq.ExactLength[ByteSeqExact10]( 10 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact10( value ); }
  final object ByteSeqExact11   extends RestrictedByteSeq.ExactLength[ByteSeqExact11]( 11 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact11( value ); }
  final object ByteSeqExact12   extends RestrictedByteSeq.ExactLength[ByteSeqExact12]( 12 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact12( value ); }
  final object ByteSeqExact13   extends RestrictedByteSeq.ExactLength[ByteSeqExact13]( 13 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact13( value ); }
  final object ByteSeqExact14   extends RestrictedByteSeq.ExactLength[ByteSeqExact14]( 14 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact14( value ); }
  final object ByteSeqExact15   extends RestrictedByteSeq.ExactLength[ByteSeqExact15]( 15 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact15( value ); }
  final object ByteSeqExact16   extends RestrictedByteSeq.ExactLength[ByteSeqExact16]( 16 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact16( value ); }
  final object ByteSeqExact17   extends RestrictedByteSeq.ExactLength[ByteSeqExact17]( 17 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact17( value ); }
  final object ByteSeqExact18   extends RestrictedByteSeq.ExactLength[ByteSeqExact18]( 18 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact18( value ); }
  final object ByteSeqExact19   extends RestrictedByteSeq.ExactLength[ByteSeqExact19]( 19 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact19( value ); }
  final object ByteSeqExact20   extends RestrictedByteSeq.ExactLength[ByteSeqExact20]( 20 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact20( value ); }
  final object ByteSeqExact21   extends RestrictedByteSeq.ExactLength[ByteSeqExact21]( 21 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact21( value ); }
  final object ByteSeqExact22   extends RestrictedByteSeq.ExactLength[ByteSeqExact22]( 22 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact22( value ); }
  final object ByteSeqExact23   extends RestrictedByteSeq.ExactLength[ByteSeqExact23]( 23 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact23( value ); }
  final object ByteSeqExact24   extends RestrictedByteSeq.ExactLength[ByteSeqExact24]( 24 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact24( value ); }
  final object ByteSeqExact25   extends RestrictedByteSeq.ExactLength[ByteSeqExact25]( 25 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact25( value ); }
  final object ByteSeqExact26   extends RestrictedByteSeq.ExactLength[ByteSeqExact26]( 26 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact26( value ); }
  final object ByteSeqExact27   extends RestrictedByteSeq.ExactLength[ByteSeqExact27]( 27 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact27( value ); }
  final object ByteSeqExact28   extends RestrictedByteSeq.ExactLength[ByteSeqExact28]( 28 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact28( value ); }
  final object ByteSeqExact29   extends RestrictedByteSeq.ExactLength[ByteSeqExact29]( 29 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact29( value ); }
  final object ByteSeqExact30   extends RestrictedByteSeq.ExactLength[ByteSeqExact30]( 30 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact30( value ); }
  final object ByteSeqExact31   extends RestrictedByteSeq.ExactLength[ByteSeqExact31]( 31 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact31( value ); }
  final object ByteSeqExact32   extends RestrictedByteSeq.ExactLength[ByteSeqExact32]( 32 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact32( value ); }
  final object ByteSeqExact64   extends RestrictedByteSeq.ExactLength[ByteSeqExact64]( 64 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact64( value ); }
  final object ByteSeqExact65   extends RestrictedByteSeq.ExactLength[ByteSeqExact65]( 65 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact65( value ); }
  final object ByteSeqExact97   extends RestrictedByteSeq.ExactLength[ByteSeqExact97]( 97 )       { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact97( value ); }
  final object ByteSeqExact194  extends RestrictedByteSeq.ExactLength[ByteSeqExact194]( 194 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact194( value ); }
  final object ByteSeqExact256  extends RestrictedByteSeq.ExactLength[ByteSeqExact256]( 256 )     { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqExact256( value ); }
  final object ByteSeqMax1024   extends RestrictedByteSeq.LimitedLength[ByteSeqMax1024]( 1024 )   { override protected def create( value : immutable.Seq[Byte] ) = new ByteSeqMax1024( value ); }

  final object StringUTF8 extends RestrictedString.NamedRestriction[StringUTF8]( StandardCharsets.UTF_8 ) { override protected def create( value : String ) = new StringUTF8( value ); }
  final object StringASCII_Exact3 extends RestrictedString.ExactLength[StringASCII_Exact3]( 3, StandardCharsets.US_ASCII ) 
  { override protected def create( value : String ) = new StringASCII_Exact3( value ); }

  /*
   * 
   * Value classes implementing types
   * 
   */
  type Int8  = RestrictedByte.AnyByte
  type Int16 = RestrictedShort.AnyShort

  final class Int24 private ( val widen : Int ) extends AnyVal with RestrictedType.Element[Int]

  type Int32 = RestrictedInt.AnyInt

  final class Int40   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int48   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int56   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int64   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int72   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int80   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int88   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int96   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int104  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int112  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int120  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int128  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int136  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int144  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int152  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int160  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int168  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int176  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int184  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int192  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int200  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int208  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int216  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int224  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int232  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int240  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int248  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]
  final class Int256  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt]

  final class UnsignedBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  final class Unsigned1   private ( val widen : Byte  )  extends AnyVal with RestrictedType.Element[Byte];
  final class Unsigned8   private ( val widen : Short  ) extends AnyVal with RestrictedType.Element[Short];
  final class Unsigned16  private ( val widen : Int    ) extends AnyVal with RestrictedType.Element[Int];
  final class Unsigned24  private ( val widen : Int    ) extends AnyVal with RestrictedType.Element[Int];
  final class Unsigned32  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned40  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned48  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned56  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned64  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned72  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned80  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned88  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned96  private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned104 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned112 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned120 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned128 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned136 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned144 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned152 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned160 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned168 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned176 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned184 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned192 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned200 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned208 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned216 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned224 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned232 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned240 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned248 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class Unsigned256 private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  
  final class Unsigned2048   private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  final class SignatureR            private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class SignatureS            private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  final class SignatureV            private ( val widen : Byte   ) extends AnyVal with RestrictedType.Element[Byte];
  final class SignatureWithChainIdV private ( val widen : Int    ) extends AnyVal with RestrictedType.Element[Int];
  final class SignatureRecId private ( val widen : Byte   ) extends AnyVal with RestrictedType.Element[Byte];

  // for solidity stubs, we need the full set of type restrictions from bytes1 to bytes32. grrr.
  // codegeneration in the REPL to the rescue...

  final class ByteSeqExact1    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact2    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact3    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact4    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact5    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact6    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact7    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact8    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact9    private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact10   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact11   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact12   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact13   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact14   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact15   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact16   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact17   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact18   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact19   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact20   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact21   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact22   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact23   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact24   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact25   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact26   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact27   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact28   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact29   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact30   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact31   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact32   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact64   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact65   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact97   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact194  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqExact256  private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  final class ByteSeqMax1024   private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];

  final class StringUTF8         private ( val widen : String ) extends AnyVal with RestrictedType.Element[String];
  final class StringASCII_Exact3 private ( val widen : String ) extends AnyVal with RestrictedType.Element[String];
}

