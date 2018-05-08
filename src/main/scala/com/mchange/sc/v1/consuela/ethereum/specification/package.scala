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
import com.mchange.sc.v1.consuela.ethereum.encoding._;

import com.mchange.sc.v2.restrict._;

import com.mchange.sc.v3.failable._;

import scala.collection._

import java.nio.charset.{Charset,StandardCharsets};

package object specification {
  private type ShieldType[BELLY] = AnyVal with RestrictedType.Element[BELLY];

  final object RestrictedTypeRLPSerializing {

    abstract class LeafElement[BELLY, SHIELD <: ShieldType[BELLY]] ( 
      eFromBelly : BELLY => RLP.Element,
      bFromBytes : immutable.Seq[Byte] => BELLY,
      factory : RestrictedType[_, BELLY, SHIELD]
    ) extends RestrictedTypeRLPSerializing[BELLY, SHIELD]( factory ) {
      def elementFromBelly( belly : BELLY )               : RLP.Element = eFromBelly( belly );
      def bellyFromElement( element : RLP.Element.Basic ) : BELLY = {
        element match {
          case RLP.Element.ByteSeq( bytes ) => bFromBytes( bytes );
          case other                        => throwNotByteSeq( other );
        }
      }
    }
    abstract class UnsignedByte[SHIELD <: ShieldType[Byte]]( factory : RestrictedType[_, Byte, SHIELD] ) extends LeafElement[Byte, SHIELD]( 
      belly => RLP.Element.UnsignedByte( belly ),
      bytes => RLP.Element.byteFromScalarBytes( bytes ),
      factory 
    );
    abstract class UnsignedShort[SHIELD <: ShieldType[Short]]( factory : RestrictedType[_, Short, SHIELD] ) extends LeafElement[Short, SHIELD]( 
      belly => RLP.Element.UnsignedShort( belly ),
      bytes => RLP.Element.shortFromScalarBytes( bytes ),
      factory 
    );
    abstract class UnsignedInt[SHIELD <: ShieldType[Int]]( factory : RestrictedType[_, Int, SHIELD] ) extends LeafElement[Int, SHIELD]( 
      belly => RLP.Element.UnsignedInt( belly ),
      bytes => RLP.Element.intFromScalarBytes( bytes ),
      factory 
    );
    abstract class UnsignedBigInt[SHIELD <: ShieldType[BigInt]]( factory : RestrictedType[_, BigInt, SHIELD] ) extends LeafElement[BigInt, SHIELD]( 
      belly => RLP.Element.UnsignedBigInt( belly ),
      bytes => BigInt( 1, bytes.toArray ),
      factory 
    );
    abstract class ByteSeq[SHIELD <: ShieldType[immutable.Seq[Byte]]]( factory : RestrictedType[_, immutable.Seq[Byte], SHIELD] ) extends LeafElement[immutable.Seq[Byte], SHIELD]( 
      belly => RLP.Element.ByteSeq( belly ),
      bytes => bytes,
      factory 
    );
    abstract class String[SHIELD <: ShieldType[java.lang.String]]( factory : RestrictedType[_, java.lang.String, SHIELD], charset : Charset ) extends LeafElement[java.lang.String, SHIELD]( 
      belly => RLP.Element.ByteSeq( belly.getBytes( charset ) ),
      bytes => new java.lang.String( bytes.toArray, charset ),
      factory 
    )
  }
  abstract class RestrictedTypeRLPSerializing[BELLY, SHIELD <: ShieldType[BELLY]]( factory : RestrictedType[_, BELLY, SHIELD] ) extends RLPSerializing[SHIELD] {
    def toElement( shield : SHIELD )               : RLP.Element      = elementFromBelly( shield.unwrap ); // boxes then unboxes, but that's okay.
    def fromElement( element : RLP.Element.Basic ) : Failable[SHIELD] = try Failable.succeed( factory( bellyFromElement( element ) ) ) catch Failable.ThrowableToFailed 

    def elementFromBelly( belly : BELLY )               : RLP.Element;
    def bellyFromElement( element : RLP.Element.Basic ) : BELLY; // let it throw its Exceptions

    protected def throwNotByteSeq( found : RLP.Element ) = throw new IllegalArgumentException( s"Expected RLP.Element.ByteSeq, found ${found}" );
  }

  implicit final object UnsignedBigInt_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.UnsignedBigInt]( Types.UnsignedBigInt );

  implicit final object Unsigned1_RLPSerializing   extends RestrictedTypeRLPSerializing.UnsignedByte[Types.Unsigned1]     ( Types.Unsigned1   );
  implicit final object Unsigned8_RLPSerializing   extends RestrictedTypeRLPSerializing.UnsignedShort[Types.Unsigned8]    ( Types.Unsigned8   );
  implicit final object Unsigned16_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedInt[Types.Unsigned16]     ( Types.Unsigned16  );
  implicit final object Unsigned24_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedInt[Types.Unsigned24]     ( Types.Unsigned24  );
  implicit final object Unsigned32_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned32]  ( Types.Unsigned32  );
  implicit final object Unsigned40_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned40]  ( Types.Unsigned40  );
  implicit final object Unsigned48_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned48]  ( Types.Unsigned48  );
  implicit final object Unsigned56_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned56]  ( Types.Unsigned56  );
  implicit final object Unsigned64_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned64]  ( Types.Unsigned64  );
  implicit final object Unsigned72_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned72]  ( Types.Unsigned72  );
  implicit final object Unsigned80_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned80]  ( Types.Unsigned80  );
  implicit final object Unsigned88_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned88]  ( Types.Unsigned88  );
  implicit final object Unsigned96_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned96]  ( Types.Unsigned96  );
  implicit final object Unsigned104_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned104] ( Types.Unsigned104 );
  implicit final object Unsigned112_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned112] ( Types.Unsigned112 );
  implicit final object Unsigned120_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned120] ( Types.Unsigned120 );
  implicit final object Unsigned128_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned128] ( Types.Unsigned128 );
  implicit final object Unsigned136_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned136] ( Types.Unsigned136 );
  implicit final object Unsigned144_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned144] ( Types.Unsigned144 );
  implicit final object Unsigned152_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned152] ( Types.Unsigned152 );
  implicit final object Unsigned160_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned160] ( Types.Unsigned160 );
  implicit final object Unsigned168_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned168] ( Types.Unsigned168 );
  implicit final object Unsigned176_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned176] ( Types.Unsigned176 );
  implicit final object Unsigned184_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned184] ( Types.Unsigned184 );
  implicit final object Unsigned192_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned192] ( Types.Unsigned192 );
  implicit final object Unsigned200_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned200] ( Types.Unsigned200 );
  implicit final object Unsigned208_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned208] ( Types.Unsigned208 );
  implicit final object Unsigned216_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned216] ( Types.Unsigned216 );
  implicit final object Unsigned224_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned224] ( Types.Unsigned224 );
  implicit final object Unsigned232_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned232] ( Types.Unsigned232 );
  implicit final object Unsigned240_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned240] ( Types.Unsigned240 );
  implicit final object Unsigned248_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned248] ( Types.Unsigned248 );
  implicit final object Unsigned256_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned256] ( Types.Unsigned256 );

  implicit final object Unsigned2048_RLPSerializing   extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned2048]  ( Types.Unsigned2048 );

  implicit final object SignatureR_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureR]( Types.SignatureR );
  implicit final object SignatureS_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureS]( Types.SignatureS );
  implicit final object SignatureV_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedByte[Types.SignatureV]  ( Types.SignatureV );

  // for solidity stubs, we need the full set of type restrictions from bytes1 to bytes32. grrr.
  // codegeneration in the REPL to the rescue...

  implicit final object ByteSeqExact1_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact1]   ( Types.ByteSeqExact1   );
  implicit final object ByteSeqExact2_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact2]   ( Types.ByteSeqExact2   );
  implicit final object ByteSeqExact3_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact3]   ( Types.ByteSeqExact3   );
  implicit final object ByteSeqExact4_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact4]   ( Types.ByteSeqExact4   );
  implicit final object ByteSeqExact5_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact5]   ( Types.ByteSeqExact5   );
  implicit final object ByteSeqExact6_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact6]   ( Types.ByteSeqExact6   );
  implicit final object ByteSeqExact7_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact7]   ( Types.ByteSeqExact7   );
  implicit final object ByteSeqExact8_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact8]   ( Types.ByteSeqExact8   );
  implicit final object ByteSeqExact9_RLPSerializing    extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact9]   ( Types.ByteSeqExact9   );
  implicit final object ByteSeqExact10_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact10]  ( Types.ByteSeqExact10  );
  implicit final object ByteSeqExact11_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact11]  ( Types.ByteSeqExact11  );
  implicit final object ByteSeqExact12_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact12]  ( Types.ByteSeqExact12  );
  implicit final object ByteSeqExact13_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact13]  ( Types.ByteSeqExact13  );
  implicit final object ByteSeqExact14_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact14]  ( Types.ByteSeqExact14  );
  implicit final object ByteSeqExact15_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact15]  ( Types.ByteSeqExact15  );
  implicit final object ByteSeqExact16_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact16]  ( Types.ByteSeqExact16  );
  implicit final object ByteSeqExact17_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact17]  ( Types.ByteSeqExact17  );
  implicit final object ByteSeqExact18_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact18]  ( Types.ByteSeqExact18  );
  implicit final object ByteSeqExact19_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact19]  ( Types.ByteSeqExact19  );
  implicit final object ByteSeqExact20_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact20]  ( Types.ByteSeqExact20  );
  implicit final object ByteSeqExact21_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact21]  ( Types.ByteSeqExact21  );
  implicit final object ByteSeqExact22_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact22]  ( Types.ByteSeqExact22  );
  implicit final object ByteSeqExact23_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact23]  ( Types.ByteSeqExact23  );
  implicit final object ByteSeqExact24_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact24]  ( Types.ByteSeqExact24  );
  implicit final object ByteSeqExact25_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact25]  ( Types.ByteSeqExact25  );
  implicit final object ByteSeqExact26_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact26]  ( Types.ByteSeqExact26  );
  implicit final object ByteSeqExact27_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact27]  ( Types.ByteSeqExact27  );
  implicit final object ByteSeqExact28_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact28]  ( Types.ByteSeqExact28  );
  implicit final object ByteSeqExact29_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact29]  ( Types.ByteSeqExact29  );
  implicit final object ByteSeqExact30_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact30]  ( Types.ByteSeqExact30  );
  implicit final object ByteSeqExact31_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact31]  ( Types.ByteSeqExact31  );
  implicit final object ByteSeqExact32_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact32]  ( Types.ByteSeqExact32  );
  implicit final object ByteSeqExact64_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact64]  ( Types.ByteSeqExact64 );
  implicit final object ByteSeqExact256_RLPSerializing  extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact256] ( Types.ByteSeqExact256 );
  implicit final object ByteSeqMax1024_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqMax1024]  ( Types.ByteSeqMax1024 );

  implicit final object StringUTF8_RLPSerializing         extends RestrictedTypeRLPSerializing.String[Types.StringUTF8]         ( Types.StringUTF8, StandardCharsets.UTF_8 );
  implicit final object StringASCII_Exact3_RLPSerializing extends RestrictedTypeRLPSerializing.String[Types.StringASCII_Exact3] ( Types.StringASCII_Exact3, StandardCharsets.US_ASCII );

}
