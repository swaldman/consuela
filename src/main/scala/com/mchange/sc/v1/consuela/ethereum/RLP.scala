package com.mchange.sc.v1.consuela.ethereum;

import scala.util.Try;
import java.nio.charset.Charset;
import com.mchange.lang.IntegerUtils;

object RLP {
  object Encoded {
    val EmptyByteSeq = encode( Encodable.EmptyByteSeq );
    val EmptySeq     = encode( Encodable.EmptySeq );
  }
  object UTF_8 {
    val _Charset = Charset.forName("UTF-8");

    /**
     * Strings interpreted as UTF-8 bytes. Strings can be empty, other empty sequences interpreted as structure
     */  
    def fastEncode( obj : Any ) : Option[Seq[Byte]] = fastEncodableWithStrings( obj, _Charset ).map( encode _ )

    def encodeString( str : String ) : Seq[Byte] = RLP.encodeString( str, _Charset )
  }
  object Encodable {
    sealed trait Basic extends Encodable;

    def sameBytes( a : Encodable, b : Encodable ) = a.simplify == b.simplify

    case class ByteSeq( bytes : scala.Seq[Byte] ) extends Encodable.Basic {
      def isSimple = true;
      def simplify = this;
    }
    case class UnsignedInt( value : scala.Int ) extends Encodable {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Encodable.ByteSeq( be( value ) )
    }
    case class UnsignedBigInt( value : scala.BigInt ) extends Encodable {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Encodable.ByteSeq( be( value ) )
    }
    case class Seq( seq : scala.Seq[Encodable] ) extends Encodable.Basic {
      lazy val isSimple = seq.forall( _.isSimple )
      def simplify = if ( this.isSimple ) this else Seq( seq.map( _.simplify ) )
    }
    val EmptyByteSeq = ByteSeq( Nil );
    val EmptySeq = Seq( Nil );
  }
  sealed trait Encodable {
    def isSimple : Boolean;
    def simplify : Encodable.Basic;
  }


  // note that encoding even the length of the length in a small portion of the first byte,
  // this encoding does not support arbitrary lengths. 
  //
  // Although it probably does support lengths sufficiently large in practice, a very very 
  // long sequence could overflow the capacity of the first byte to encode the length of its 
  // length.

  /**
   *  @return a pair, decoded Encodable and unconsumed bytes
   */  
  def decode( bytes : Seq[Byte] ) : (Encodable.Basic, Seq[Byte]) = {
    def splitOut( splitMe : Seq[Byte], len : Int ) = {
      val ( decoded, rest ) = splitMe.splitAt( len );
      ( Encodable.ByteSeq( decoded ), rest )
    }
    def splitOutSeq( splitMe : Seq[Byte], len : Int ) = {
      val (encodables, rest) = encodableSeq( len, Nil, splitMe );
      ( Encodable.Seq( encodables ), rest )
    }
    def decodeLengthBytes( lengthBytes : Seq[Byte] ) : Int = {
      if ( lengthBytes.length == 0 ) {
        0
      } else {
        val bi = BigInt(1, lengthBytes.toArray);
        if ( bi > java.lang.Integer.MAX_VALUE )
          throw new AssertionError("We only support decoding of sequences no longer than ${java.lang.Integer.MAX_VALUE}, but desired length is ${bi}.");
        bi.intValue;
      }
    }
    def encodableSeq( count : Int, reverseAccum : List[Encodable.Basic], in : Seq[Byte] ) : ( Seq[Encodable.Basic], Seq[Byte] ) = {
      assert( 
        count >= 0,
        s"Count should begin with precisely the byte length of the concatenated elements, and so we should end with precisely zero bytes. count -> ${count}, reverseAccum -> ${reverseAccum}" 
      );

      if ( count == 0 )
        ( reverseAccum.reverse, in );
      else {
        val ( encodable, rest ) = decode( in );
        val eaten = in.length - rest.length
        encodableSeq( count - eaten, encodable :: reverseAccum, rest );
      }
    }

    val discriminator  = bytes(0) & 0xFF;
    if ( discriminator > 247 ) {
      val seqLenBytesLen = discriminator - 247;
      val (seqLenBytes, rest) = bytes.tail.splitAt( seqLenBytesLen );
      val seqLen = decodeLengthBytes( seqLenBytes );
      splitOutSeq( rest, seqLen )
    } else if ( discriminator >= 192 ) { //note that zero-length lists get encoded here, so the boundary is inclusive
      val seqLen = discriminator - 192;
      splitOutSeq( bytes.tail, seqLen )
    } else if ( discriminator > 183 ) {
      val eatLenBytesLen = discriminator - 183;
      val (eatLenBytes, rest) = bytes.tail.splitAt( eatLenBytesLen );
      val eatLen = decodeLengthBytes( eatLenBytes );
      splitOut( rest, eatLen )
    } else if ( discriminator >= 128 ) { //note that zero-length strings get encoded here, so the boundary is inclusive
      val eatLen = discriminator - 128;
      splitOut( bytes.tail, eatLen )
    } else {
      splitOut(bytes, 1)
    }
  }
  def encode( encodable : Encodable ) : Seq[Byte] = {
    def rb( bs : Seq[Byte] ) : Seq[Byte] = {

      // we pass Ints, because Bytes coerced to Ints are unsigned (as we
      // expect and require, and because the JVM'll promote them to Ints
      // anyway during various operations.

      def _rb( bytes : Seq[Int] ) : Seq[Byte] = {
        lazy val len = bytes.length;
        bytes match {
          case Seq( b ) if ( b < 128 )  => Seq( b.toByte );
          case _        if ( len < 56 ) => (((128 + len) +: bytes) : Seq[Int]).map( _.toByte );
          case _                        => {
            val lenBytes = be( len );
            (183 + lenBytes.length).toByte +: (lenBytes ++: bytes.map(  _.toByte ))
          }
        }
      }
      def _promote( bytes : Seq[Byte] ) : Seq[Int] = bytes.map( _ & 0xFF );

      _rb( _promote( bs ) );
    }
    def rl( encodables : Seq[Encodable] ) : Seq[Byte] = {

      // note: this was a surpise to me. the length of s(x), described in the ethereum yellow paper as ||s(x)|| is
      // the *byte length of the serialized representation of all concatenated members of the sequence*, 
      // not the length of the top-level sequence, i.e. not the number of members the will be concatenated into
      // the sequence.

      val sencodables = s( encodables );
      val len = sencodables.length;
      if ( len < 56 )
        (192 + len).toByte +: sencodables
      else {
        val lenBytes = be( len );
        (247 + lenBytes.length).toByte +: ( lenBytes ++: sencodables )
      }
    }
    def s( encodables : Seq[Encodable] ) : Seq[Byte] = encodables.flatMap( encode _ );

    encodable match {
      case bse : Encodable.ByteSeq        => rb( bse.bytes );
      case ie  : Encodable.UnsignedInt    => rb( be( ie.value ) );
      case bie : Encodable.UnsignedBigInt => rb( be( bie.value ) );
      case ese : Encodable.Seq            => rl( ese.seq );
    }
  }
  def encodeBytes( bytes : Seq[Byte] )                : Seq[Byte] = encode( Encodable.ByteSeq( bytes ) );
  def encodeUnsignedInt( i : Int )                    : Seq[Byte] = encode( Encodable.UnsignedInt( i ) );
  def encodeUnsignedBigInt( bi : BigInt )             : Seq[Byte] = encode( Encodable.UnsignedBigInt( bi ) );
  def encodeString( str : String, charset : Charset ) : Seq[Byte] = encode( Encodable.ByteSeq( str.getBytes( charset ).toSeq ) );
  /**
   * Strings can be empty, other empty sequences interpreted as structure
   */  
  def fastEncodableWithStrings( obj : Any, charset : Charset ) : Option[Encodable] = {
    def tryAsAtom : Option[Encodable] = {
      obj match {
        case i   : Int    if (i >= 0)  => Some( Encodable.UnsignedInt( i ) )
        case bi  : BigInt if (bi >= 0) => Some( Encodable.UnsignedBigInt( bi ) )
        case str : String              => Some( Encodable.ByteSeq( str.getBytes( charset ) ) )
        case _                         => None
      }
    }
    def tryAsSeq : Option[Encodable] = {
      if ( obj.isInstanceOf[Seq[_]] ) {
        val seq : Seq[_] = obj.asInstanceOf[Seq[_]];

        def tryAsHomogenousByteSeq : Option[Encodable] = {
          seq match {
            case Seq() => Some( Encodable.Seq( Seq.empty[Encodable] ) );
            case Seq( head, tail @ _* ) => {
              head match {
                case byte : Byte => {
                  if ( tail.forall( _.isInstanceOf[Byte] ) ) { // yay! Seq[Byte]
                    Some( Encodable.ByteSeq( byte +: tail.map( _.asInstanceOf[Byte] ) ) )
                  } else {
                    None
                  }
                }
                case _ => None
              }
            }
            case _ => None
          }
        }
        def tryAsOtherSeq : Option[Encodable] = Try( Encodable.Seq( seq.map( fastEncodableWithStrings( _, charset ).get ) ) ).toOption

        tryAsHomogenousByteSeq orElse tryAsOtherSeq
      } else {
        None
      }
    }

    tryAsAtom orElse tryAsSeq
  }

  private[this] def be( i : Int ) = IntegerUtils.byteArrayFromInt( i ).dropWhile( _ == 0 );
  private[this] def be( bi : BigInt ) = bi.toByteArray.dropWhile( _ == 0 );
}

