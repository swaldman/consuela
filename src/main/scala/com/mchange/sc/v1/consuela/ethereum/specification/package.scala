package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum.encoding._;

import com.mchange.sc.v2.restrict._;

import com.mchange.sc.v2.failable._;

import scala.collection._

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
  }
  abstract class RestrictedTypeRLPSerializing[BELLY, SHIELD <: ShieldType[BELLY]]( factory : RestrictedType[_, BELLY, SHIELD] ) extends RLPSerializing[SHIELD] {
    def toElement( shield : SHIELD )               : RLP.Element      = elementFromBelly( shield.unwrap ); // boxes then unboxes, but that's okay.
    def fromElement( element : RLP.Element.Basic ) : Failable[SHIELD] = try succeed( factory( bellyFromElement( element ) ) ) catch Poop; 

    def elementFromBelly( belly : BELLY )               : RLP.Element;
    def bellyFromElement( element : RLP.Element.Basic ) : BELLY; // let it throw its Exceptions

    protected def throwNotByteSeq( found : RLP.Element ) = throw new IllegalArgumentException( s"Expected RLP.Element.ByteSeq, found ${found}" );
  }

  implicit final object UnsignedBigInt_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.UnsignedBigInt]( Types.UnsignedBigInt );
  implicit final object Unsigned8_RLPSerializing      extends RestrictedTypeRLPSerializing.UnsignedShort[Types.Unsigned8]      ( Types.Unsigned8      );
  implicit final object Unsigned64_RLPSerializing     extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned64]    ( Types.Unsigned64     );
  implicit final object Unsigned256_RLPSerializing    extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned256]   ( Types.Unsigned256    );
  implicit final object Unsigned2048_RLPSerializing   extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned2048]  ( Types.Unsigned2048   );

  implicit final object SignatureR_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureR]( Types.SignatureR );
  implicit final object SignatureS_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureS]( Types.SignatureS );
  implicit final object SignatureV_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedByte[Types.SignatureV]  ( Types.SignatureV );

  implicit final object ByteSeqExact4_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact4]  ( Types.ByteSeqExact4  );
  implicit final object ByteSeqExact8_RLPSerializing   extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact8]  ( Types.ByteSeqExact8  );
  implicit final object ByteSeqExact20_RLPSerializing  extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact20] ( Types.ByteSeqExact20 );
  implicit final object ByteSeqExact32_RLPSerializing  extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact32] ( Types.ByteSeqExact32 );
  implicit final object ByteSeqExact256_RLPSerializing extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact256]( Types.ByteSeqExact256 );
  implicit final object ByteSeqMax1024_RLPSerializing  extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqMax1024] ( Types.ByteSeqMax1024 );
}
