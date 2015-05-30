package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

import com.mchange.sc.v2.failable._;

import scala.collection._;

package object encoding {
  /*
   * 
   * Nibble stuff
   * 
   */ 
  type Nibble = Int;

  val Nibbles = (0x0 to 0xF).toIndexedSeq

  def isNibble( mbNibble : Int ) = ( mbNibble >= 0x0 && mbNibble <= 0xF )

  /**
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */  
  def toNibbles( bytes : Seq[Byte] ) : IndexedSeq[Nibble] = leastSignificantByteNibbles( bytes.map( _ & 0xFF ) )

  def toNibbles( key : String, charsetStr : String ) : IndexedSeq[Nibble] = toNibbles( key.getBytes( charsetStr ) );

  def toNibbles( key : String ) : IndexedSeq[Nibble] = toNibbles( key, "UTF-8" );

  def nibblesToBytes( nibbles : Seq[Nibble] ) : Seq[Byte] = {
    def bytify( pairAsSeq : Seq[Nibble] ) : Byte = ( (pairAsSeq.head << 4) | (pairAsSeq.last << 0) ).toByte;
    require( nibbles.length % 2 == 0, s"Only an even number of nibbles can be concatenated into bytes! nibbles.length -> ${nibbles.length}" );
    nibbles.sliding(2,2).map( bytify _ ).toSeq;
  }

  /**
   *  Least significant byte nibbles, ignores top three bytes!
   * 
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */ 
  def leastSignificantByteNibbles( bytes : Seq[Int] ) : IndexedSeq[Nibble] = {
    val out = new Array[Int]( bytes.length * 2 );
    var idx = 0;
    bytes.foreach {  byte => 
      out(idx) = (byte & 0xF0) >>> 4;
      out(idx+1) = byte & 0x0F;
      idx += 2
    }
    out.toIndexedSeq
  }

  /*
   * 
   * Basic RLP serialization implicits
   * 
   */ 
  // serializers
  implicit final object ByteSeqSerializer extends RLPSerializing[Seq[Byte]] {
    def toElement( bytes : Seq[Byte] )             : RLP.Element = RLP.Element.ByteSeq( bytes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Seq[Byte]] = element match {
      case RLP.Element.ByteSeq( bytes ) => succeed( bytes );
      case _                            => failNotLeaf( element );
    }
  }
  implicit final object ImmutableByteSeqSerializer extends RLPSerializing[immutable.Seq[Byte]] {
    def toElement( bytes : immutable.Seq[Byte] )   : RLP.Element = RLP.Element.ByteSeq( bytes );
    def fromElement( element : RLP.Element.Basic ) : Failable[immutable.Seq[Byte]] = element match {
      case RLP.Element.ByteSeq( bytes ) => succeed( bytes );
      case _                            => failNotLeaf( element );
    }
  }
  implicit final object ByteArraySerializer extends RLPSerializing[Array[Byte]] {
    def toElement( bytes : Array[Byte] )           : RLP.Element = RLP.Element.ByteSeq( bytes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Array[Byte]] = element match {
      case RLP.Element.ByteSeq( bytes ) => succeed( bytes.toArray );
      case _                            => failNotLeaf( element );
    }
  }
  /*
  implicit final object UnsignedIntSerializer extends RLPSerializing[Int] {
    def toElement( i : Int )                       : RLP.Element = RLP.Element.UnsignedInt( i );
    def fromElement( element : RLP.Element.Basic ) : Failable[Int] = element match {
      case RLP.Element.ByteSeq( bytes ) if (bytes.length <= 4) => {
        val result = RLP.Element.intFromScalarBytes(bytes);
        if ( result >= 0 ) {
          succeed( result ) 
        } else {
          fail( 
            s"Desired int would be negative (${result}); RLP scalars must be nonnegative. " +
             "Perhaps this represents an unigned value too large to fit in a for byte Int. " +
             "If negative values are expected, applications may read an interpret the byte sequence directly.")
        }
      }
      case RLP.Element.ByteSeq( bytes ) if (bytes.length > 4)  => fail( "The Int value requested cannot be represented in an 4 byte Int." );
      case _                                                   => failNotLeaf( element );
    }
  }
  implicit final object UnsignedBigIntSerializer extends RLPSerializing[BigInt] {
    def toElement( bi : BigInt )                   : RLP.Element = RLP.Element.UnsignedBigInt( bi );
    def fromElement( element : RLP.Element.Basic ) : Failable[BigInt] = element match {
      case RLP.Element.ByteSeq( bytes ) => succeed( BigInt( 1, bytes.toArray ) );
      case _                            => failNotLeaf( element );
    }
  }
  */
 
  // pimps
  final object RLPOps {
    trait Lazy[T] { // classes can implement RLPOps.Lazy if they wish to implement offer on-structure memoization
      this : T =>

      val rlpOpsView : T => RLPOps[T];

      lazy val rlpElement : RLP.Element         = rlpOpsView( this ).rlpElement;
      lazy val rlpBytes   : immutable.Seq[Byte] = RLP.Element.encode( this.rlpElement );
    }
  }
  implicit class RLPOps[ T : RLPSerializing ]( rlpSerializable : T ) {
    def rlpElement : RLP.Element         = implicitly[RLPSerializing[T]].toElement( rlpSerializable.asInstanceOf[T] );
    def rlpBytes   : immutable.Seq[Byte] = implicitly[RLPSerializing[T]].encode( this.rlpSerializable );
  }
}
