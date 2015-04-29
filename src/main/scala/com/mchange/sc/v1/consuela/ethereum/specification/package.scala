package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum.encoding._;

import com.mchange.sc.v2.restrict._;

import scala.collection._

package object specification {
  private type ShieldType[BELLY] = AnyVal with RestrictedType.Element[BELLY];

  object RestrictedTypeRLPSerializing {

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
    def fromElement( element : RLP.Element.Basic ) : Failable[SHIELD] = try succeed( factory( bellyFromElement( element ) ) ) catch poop; 

    def elementFromBelly( belly : BELLY )               : RLP.Element;
    def bellyFromElement( element : RLP.Element.Basic ) : BELLY; // let it throw its Exceptions

    protected def throwNotByteSeq( found : RLP.Element ) = throw new IllegalArgumentException( s"Expected RLP.Element.ByteSeq, found ${found}" );
  }

  implicit object Unsigned_RLPSerializing     extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned]    ( Types.Unsigned     );
  implicit object Unsigned8_RLPSerializing    extends RestrictedTypeRLPSerializing.UnsignedShort[Types.Unsigned8]    ( Types.Unsigned8    );
  implicit object Unsigned256_RLPSerializing  extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned256] ( Types.Unsigned256  );
  implicit object Unsigned2048_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.Unsigned2048]( Types.Unsigned2048 );

  implicit object SignatureR_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureR]( Types.SignatureR );
  implicit object SignatureS_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedBigInt[Types.SignatureS]( Types.SignatureS );
  implicit object SignatureV_RLPSerializing extends RestrictedTypeRLPSerializing.UnsignedByte[Types.SignatureV]  ( Types.SignatureV );

  implicit object ByteSeqExact4_RLPSerializing extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact4]  ( Types.ByteSeqExact4  );
  implicit object ByteSeqExact8_RLPSerializing extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqExact8]  ( Types.ByteSeqExact8  );
  implicit object ByteSeqMax1024_RLPSerializing extends RestrictedTypeRLPSerializing.ByteSeq[Types.ByteSeqMax1024]( Types.ByteSeqMax1024 );
}
