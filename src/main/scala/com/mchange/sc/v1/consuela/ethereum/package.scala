package com.mchange.sc.v1.consuela;

import com.mchange.lang.IntegerUtils;

package object ethereum {

  object HP {
    def encode( nibbles : Seq[Int], flag : Boolean ) : Seq[Byte] = {
      require( nibbles.forall( _ < 16 ), s"nibbles should be values between 0 and 15 [ nibbles -> ${ nibbles} ]" );
      _encode( nibbles, flag ).map( _.asInstanceOf[Byte] )
    }
    private[this] def _encode( nibbles : Seq[Int], flag : Boolean ) : Seq[Int] = {
      val f = if ( flag ) 2 else 0;
      val len = nibbles.length;
      val even = (len % 2 == 0);
      def combine( start : Int ) : Int = nibbles( start ) << 4 + nibbles( start + 1 );
      def reverseBuild( accum : List[Int], start : Int ) : List[Int] = {
        if ( start < len ) reverseBuild( combine( start ) :: accum, start + 2 );
        else accum;
      }
      if ( even ) {
        val headerByte = f << 4;
        reverseBuild( headerByte :: Nil, 0 ).reverse
      } else {
        val headerByte = ((f + 1) << 4) + nibbles(0);
        reverseBuild( headerByte :: Nil, 1 ).reverse
      }
    }
  }

  object RLP {
    object Encodable {
      case class ByteSeq( bytes : scala.Seq[Byte] ) extends Encodable with Structure;
      case class Int( value : scala.Int ) extends Encodable;
      case class BigInt( value : scala.BigInt ) extends Encodable;
      case class Seq( seq : scala.Seq[Encodable] ) extends Encodable with Structure;
    }
    sealed trait Encodable;
    trait Structure;


    // note that encoding even the length of the length in a small portion of the first byte
    // does not support arbitrary lengths. Although it probably does support lengths sufficiently
    // large in practice, a sufficiently long sequence could overflow the capacity of the first
    // byte to encode the length of its length.

    def decode( bytes : Seq[Byte] ) : (Encodable with Structure, Seq[Byte]) = {
      def splitOut( splitMe : Seq[Byte], len : Int ) = {
        val ( decoded, rest ) = splitMe.splitAt( len );
        ( Encodable.ByteSeq( decoded ), rest )
      }
      def splitOutSeq( splitMe : Seq[Byte], len : Int ) = {
        val (encodables, rest) = encodableSeq( len, Nil, splitMe );
        ( Encodable.Seq( encodables ), rest )
      }
      def decodeLengthBytes( lengthBytes : Seq[Byte] ) : Int = {
        val bi = BigInt(lengthBytes.toArray);
        if ( bi > java.lang.Integer.MAX_VALUE )
          throw new AssertionError("We only support decoding of sequences no longer than ${java.lang.Integer.MAX_VALUE}, but desired length is ${bi}.");
        bi.intValue;
      }
      def encodableSeq( count : Int, reverseAccum : List[Encodable with Structure], in : Seq[Byte] ) : ( Seq[Encodable with Structure], Seq[Byte] ) = {
        if ( count == 0 )
          ( reverseAccum.reverse, in );
        else {
          val ( encodable, rest ) = decode( in );
          encodableSeq( count - 1, encodable :: reverseAccum, rest );
        }
      }

      val discriminator  = bytes(0).asInstanceOf[Int];
      if ( discriminator < 128 )
        splitOut(bytes, 1)
      else {
        val discriminibble = discriminator >>> 4;
        discriminibble match {
          case 8 => {
            val eatLen = discriminator - 128;
            splitOut( bytes.tail, eatLen )
          }
          case 11 => {
            val eatLenBytesLen = discriminator - 183;
            val (eatLenBytes, rest) = bytes.tail.splitAt( eatLenBytesLen );
            val eatLen = decodeLengthBytes( eatLenBytes );
            splitOut( rest, eatLen )
          }
          case 12 => {
            val seqLen = discriminator - 192;
            splitOutSeq( bytes.tail, seqLen )
          }
          case 15 => {
            val seqLenBytesLen = discriminator - 247;
            val (seqLenBytes, rest) = bytes.tail.splitAt( seqLenBytesLen );
            val seqLen = decodeLengthBytes( seqLenBytes );
            splitOutSeq( rest, seqLen )
          }
          case _ => {
            throw new AssertionError( s"Invalid first byte: ${discriminator} (with first nibble ${discriminibble}, legal values: 8, 11, 12, 15)." )
          }
        }
      }
    }
    def encode( encodable : Encodable ) : Seq[Byte] = {
      encodable match {
        case bse : Encodable.ByteSeq => rb( bse.bytes );
        case ie  : Encodable.Int     => rb( be( ie.value ) );
        case bie : Encodable.BigInt  => rb( be( bie.value ) );
        case ese : Encodable.Seq     => rl( ese.seq );
      }
    }
    def rb( bs : Seq[Byte] ) : Seq[Byte] = {

      // we pass Ints, because Bytes coerced to Ints are unsigned (as we
      // expect and require, and because the JVM'll promote them to Ints
      // anyway during various operations.

      def _rb( bytes : Seq[Int] ) : Seq[Byte] = {
        lazy val len = bytes.length;
        bytes match {
          case Seq( b ) if ( b < 128 )  => Seq( b.asInstanceOf[Byte] );
          case _        if ( len < 56 ) => ((128 + len) +: bytes).map( _.asInstanceOf[Byte] );
          case _                        => {
            val lenBytes = be( len );
            ((183 + lenBytes.length) +: (lenBytes ++: bytes)).map( _.asInstanceOf[Byte] );
          }
        }
      }
      def _promote( bytes : Seq[Byte] ) : Seq[Int] = bytes.map( _.asInstanceOf[Int] );

      _rb( _promote( bs ) );
    }
    def rl( encodables : Seq[Encodable] ) : Seq[Byte] = {
      val len = encodables.length;
      if ( len < 56 )
        ((192 + len) +: s( encodables )).map( _.asInstanceOf[Byte] );
      else {
        val lenBytes = be( len );
        ((247 + lenBytes.length) +: ( lenBytes ++: s(encodables) )).map( _.asInstanceOf[Byte] );
      }
    }
    def s( encodables : Seq[Encodable] ) : Seq[Byte] = encodables.flatMap( encode _ );
    def be( i : Int ) = IntegerUtils.byteArrayFromInt( i ).dropWhile( _ == 0 );
    def be( bi : BigInt ) = bi.toByteArray.dropWhile( _ == 0 );
  }
}
