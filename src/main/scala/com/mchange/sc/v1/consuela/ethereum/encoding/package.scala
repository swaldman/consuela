package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

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
  implicit object ByteSeqSerializer extends RLPSerializing[Seq[Byte]] {
    def toRLPEncodable( bytes : Seq[Byte] )                 : RLP.Encodable = RLP.Encodable.ByteSeq( bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[Seq[Byte]] = encodable match {
      case RLP.Encodable.ByteSeq( bytes ) => succeed( bytes );
      case _                              => failNotLeaf( encodable );
    }
  }
  implicit object ImmutableByteSeqSerializer extends RLPSerializing[immutable.Seq[Byte]] {
    def toRLPEncodable( bytes : immutable.Seq[Byte] )       : RLP.Encodable = RLP.Encodable.ByteSeq( bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[immutable.Seq[Byte]] = encodable match {
      case RLP.Encodable.ByteSeq( bytes ) => succeed( bytes );
      case _                              => failNotLeaf( encodable );
    }
  }
  implicit object ByteArraySerializer extends RLPSerializing[Array[Byte]] {
    def toRLPEncodable( bytes : Array[Byte] )               : RLP.Encodable = RLP.Encodable.ByteSeq( bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[Array[Byte]] = encodable match {
      case RLP.Encodable.ByteSeq( bytes ) => succeed( bytes.toArray );
      case _                              => failNotLeaf( encodable );
    }
  }
  implicit object UnsignedIntSerializer extends RLPSerializing[Int] {
    def toRLPEncodable( i : Int )                           : RLP.Encodable = RLP.Encodable.UnsignedInt( i );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[Int] = encodable match {
      case RLP.Encodable.ByteSeq( bytes ) if (bytes.length <= 4) => {
        val result = RLP.Encodable.intFromScalarBytes(bytes);
        if ( result >= 0 ) {
          succeed( result ) 
        } else {
          fail( 
            s"Desired int would be negative (${result}); RLP scalars must be nonnegative. " +
             "Perhaps this represents an unigned value too large to fit in a for byte Int. " +
             "If negative values are expected, applications may read an interpret the byte sequence directly.")
        }
      }
      case RLP.Encodable.ByteSeq( bytes ) if (bytes.length > 4)  => fail( "The Int value requested cannot be represented in an 4 byte Int." );
      case _                                                     => failNotLeaf( encodable );
    }
  }
  implicit object UnsignedBigIntSerializer extends RLPSerializing[BigInt] {
    def toRLPEncodable( bi : BigInt )                       : RLP.Encodable = RLP.Encodable.UnsignedBigInt( bi );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[BigInt] = encodable match {
      case RLP.Encodable.ByteSeq( bytes ) => succeed( BigInt( 1, bytes.toArray ) );
      case _                              => failNotLeaf( encodable );
    }
  }

  // pimps
  object RLPOps {
    trait Lazy[T] { // classes can implement RLPOps.Lazy if they wish to implement offer on-structure memoization
      this : T =>

      val rlpOpsView : T => RLPOps[T];

      lazy val rlpEncodable : RLP.Encodable       = rlpOpsView( this ).rlpEncodable;
      lazy val rlpBytes     : immutable.Seq[Byte] = RLP.Encodable.encode( this.rlpEncodable );
    }
  }
  implicit class RLPOps[ T : RLPSerializing ]( rlpSerializable : T ) {
    def rlpEncodable : RLP.Encodable       = implicitly[RLPSerializing[T]].toRLPEncodable( rlpSerializable.asInstanceOf[T] );
    def rlpBytes     : immutable.Seq[Byte] = implicitly[RLPSerializing[T]].encodeRLP( this.rlpSerializable );
  }
}
