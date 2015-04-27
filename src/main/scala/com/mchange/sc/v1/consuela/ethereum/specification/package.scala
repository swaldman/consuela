package com.mchange.sc.v1.consuela.ethereum;

package object specification {
  import Types._;

  implicit object UnsignedInt_RLPSerializer extends RLPSerializing[UnsignedInt] {
    def toElement( ui : UnsignedInt )              : RLP.Element = RLP.Element.UnsignedInt( ui.value );
    def fromElement( element : RLP.Element.Basic ) : Failable[UnsignedInt] = element match {
      case RLP.Element.ByteSeq( bytes ) if (bytes.length <= 4) => {
        val result = RLP.Element.intFromScalarBytes(bytes);
        try {
          succeed( UnsignedInt( result ) )
        } catch {
          case e : Exception => fail( e )
        }
      }
      case RLP.Element.ByteSeq( bytes ) if (bytes.length > 4)  => fail( "The Int value requested cannot be represented in a 4 byte Int." );
      case _                                                   => failNotLeaf( element );
    }
  }
  implicit object UnsignedLong_RLPSerializer extends RLPSerializing[UnsignedLong] {
    def toElement( ul : UnsignedLong )             : RLP.Element = RLP.Element.UnsignedLong( ul.value );
    def fromElement( element : RLP.Element.Basic ) : Failable[UnsignedInt] = element match {
      case RLP.Element.ByteSeq( bytes ) if (bytes.length <= 8) => {
        val result = RLP.Element.longFromScalarBytes(bytes);
        try {
          succeed( UnsignedLong( result ) )
        } catch {
          case e : Exception => fail( e )
        }
      }
      case RLP.Element.ByteSeq( bytes ) if (bytes.length > 8)  => fail( "The Long value requested cannot be represented in an 8 byte Long." );
      case _                                                   => failNotLeaf( element );
    }
  }
  implicit object Unsigned_RLPSerializer extends RLPSerializing[Unsigned] {
    def toElement( u : Unsigned )                  : RLP.Element = RLP.Element.UnsignedBigInt( u.value );
    def fromElement( element : RLP.Element.Basic ) : Failable[BigInt] = element match {
      /*
       *  Note that as RLP "scalars" are defined, we cannot distinguish between an
       *  erroneous negative value and a correct unsigned value from which zeroes have
       *  been dropped. So we must accept any RLP-encoded value as correct, and add
       *  (hopefully back) leading zeroes as necessary to render them unsigned.
       */ 
      case RLP.Element.ByteSeq( bytes ) => succeed( BigInt( 1, bytes.toArray ) );
      case _                            => failNotLeaf( element );
    }
  }
  implicit object Unsigned8_RLPSerializer extends RLPSerializing[Unsigned8] {
    def toElement( u8 : Unsigned8 )                : RLP.Element         = RLP.Element.UnsignedBigInt( u8.value );
    def fromElement( element : RLP.Element.Basic ) : Failable[Unsigned8] = {
      try { Unsigned_RLPSerializer.fromElement( element ).toIntExact;
    }
  }
}
