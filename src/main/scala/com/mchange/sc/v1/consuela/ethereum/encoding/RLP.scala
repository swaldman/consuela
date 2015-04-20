package com.mchange.sc.v1.consuela.ethereum.encoding;

import scala.util.Try;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import com.mchange.lang.IntegerUtils;

import scala.collection._;
import scala.reflect.ClassTag;
import com.mchange.sc.v1.consuela.util.ImmutableArraySeq;

object RLP {


  def toEncodable[T : RLPSerializer]( t : T )                             : RLP.Encodable = implicitly[RLPSerializer[T]].toRLPEncodable( t );
  def fromEncodable[T : RLPSerializer]( encodable : RLP.Encodable.Basic ) : Option[T]     = implicitly[RLPSerializer[T]].fromRLPEncodable( encodable );

  def decode[T : RLPSerializer]( bytes : Seq[Byte] )                    : ( Option[T], Seq[Byte] ) = implicitly[RLPSerializer[T]].decodeRLP( bytes );
  def decodeComplete[T : RLPSerializer]( bytes : Seq[Byte] )            : Option[T]                = implicitly[RLPSerializer[T]].decodeCompleteRLP( bytes ); 
  def encode[T : RLPSerializer]( t : T )                                : immutable.Seq[Byte]      = implicitly[RLPSerializer[T]].encodeRLP( t ); 

  // basic serializers
  implicit object ByteSeqSerializer extends RLPSerializer[Seq[Byte]] {
    def toRLPEncodable( bytes : Seq[Byte] )                 : RLP.Encodable     = Encodable.ByteSeq( bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[Seq[Byte]] = encodable match {
      case Encodable.ByteSeq( bytes ) => Some( bytes );
      case _                          => None;
    }
  }
  implicit object UnsignedIntSerializer extends RLPSerializer[Int] {
    def toRLPEncodable( i : Int )                           : RLP.Encodable = Encodable.UnsignedInt( i );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[Int]     = encodable match {
      case Encodable.ByteSeq( bytes ) if (bytes.length <= 8) => Some( Encodable.unbeInt(bytes) ).filter( _ >= 0 );
      case _                                                 => None;
    }
  }
  implicit object UnsignedBigIntSerializer extends RLPSerializer[BigInt] {
    def toRLPEncodable( bi : BigInt )                       : RLP.Encodable = Encodable.UnsignedBigInt( bi );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[BigInt] = encodable match {
      case Encodable.ByteSeq( bytes ) => Some( BigInt( 1, bytes.toArray ) );
      case _                          => None;
    }
  }

  // convenience methods
  def encodeString( str : String, charset : Charset ) : immutable.Seq[Byte] = Encodable.encode( Encodable.ByteSeq( str.getBytes( charset ).toSeq ) );

  object Encoded {
    val EmptyByteSeq = Encodable.encode( Encodable.EmptyByteSeq );
    val EmptySeq     = Encodable.encode( Encodable.EmptySeq );
  }
  object UTF_8 {
    val _Charset = StandardCharsets.UTF_8;

    /**
     * Strings interpreted as UTF-8 bytes. Strings can be empty, other empty sequences interpreted as structure
     */  
    def fastEncode( obj : Any ) : Option[Seq[Byte]] = Encodable.fastEncodableWithStrings( obj, _Charset ).map( Encodable.encode _ )

    def encodeString( str : String ) : Seq[Byte] = RLP.encodeString( str, _Charset )
  }

  /**
   * Encodables are now formally immutable.
   */ 
  object Encodable {

    sealed trait Basic extends Encodable;

    def sameBytes( a : Encodable, b : Encodable ) = a.simplify == b.simplify

    object ByteSeq {
      def apply( array : Array[Byte] )   : ByteSeq = new ByteSeq( ImmutableArraySeq.Byte( array ) );
      def apply( seq : scala.Seq[Byte] ) : ByteSeq = new ByteSeq( toImmutableBytes( seq ) );
    }
    case class ByteSeq( bytes : immutable.Seq[Byte] ) extends Encodable.Basic {
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
    object Seq {
      def of( encodables : Encodable* )       = new Seq( ImmutableArraySeq( encodables.toArray ) );
      def apply( array : Array[Encodable] )   = new Seq( ImmutableArraySeq( array ) );
      def apply( seq : scala.Seq[Encodable] ) = new Seq( toImmutable( seq ) );
    }
    case class Seq( seq : immutable.Seq[Encodable] ) extends Encodable.Basic {
      lazy val isSimple = seq.forall( _.isSimple )
      def simplify = if ( this.isSimple ) this else Seq( seq.map( _.simplify ) )
    }
    val EmptyByteSeq = ByteSeq( Nil );
    val EmptySeq = Seq( Nil );

    private def toImmutable[T : ClassTag]( seq : scala.Seq[T] ) : immutable.Seq[T] = {
      seq match {
        case list   : scala.List[T]           => list;
        case vector : immutable.Vector[T]     => vector;
        case lb     : mutable.ListBuffer[T]   => lb.toList;
        case wa     : mutable.WrappedArray[T] => ImmutableArraySeq( wa.toArray );
        case other                            => immutable.Vector( other : _* );
      }
    }

    private def toImmutableBytes( seq : scala.Seq[Byte] ) : immutable.Seq[Byte] = {
      seq match {
        case wa : mutable.WrappedArray[Byte] => ImmutableArraySeq.Byte( wa.toArray );
        case other                           => toImmutable( other );
      }
    }



    // note that encoding even the length of the length in a small portion of the first byte,
    // this encoding does not support arbitrary lengths.
    //
    // Although it probably does support lengths sufficiently large in practice, a very very
    // long sequence could overflow the capacity of the first byte to encode the length of its
    // length.

    def decodeComplete( bytes : scala.Seq[Byte] ) : Encodable.Basic = decode( bytes ).ensuring( _._2.isEmpty )._1

    /**
     *  @return a pair, decoded Encodable and unconsumed bytes
     */  
    def decode( bytes : scala.Seq[Byte] ) : (Encodable.Basic, scala.Seq[Byte]) = {
      def splitOut( splitMe : scala.Seq[Byte], len : Int ) = {
        val ( decoded, rest ) = splitMe.splitAt( len );
        ( Encodable.ByteSeq( decoded ), rest )
      }
      def splitOutSeq( splitMe : scala.Seq[Byte], len : Int ) = {
        val (encodables, rest) = encodableSeq( len, Nil, splitMe );
        ( Encodable.Seq( encodables ), rest )
      }
      def decodeLengthBytes( lengthBytes : scala.Seq[Byte] ) : Int = {
        if ( lengthBytes.length == 0 ) {
          0
        } else {
          val bi = BigInt(1, lengthBytes.toArray);
          if ( bi > java.lang.Integer.MAX_VALUE )
            throw new AssertionError("We only support decoding of sequences no longer than ${java.lang.Integer.MAX_VALUE}, but desired length is ${bi}.");
          bi.intValue;
        }
      }
      def encodableSeq( count : Int, reverseAccum : List[Encodable.Basic], in : scala.Seq[Byte] ) : ( scala.Seq[Encodable.Basic], scala.Seq[Byte] ) = {
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
    def encode( encodable : Encodable ) : scala.collection.immutable.Seq[Byte] = {

      def rba( arr : Array[Byte] ) : immutable.Seq[Byte] = rb( ImmutableArraySeq.Byte( arr ) )

      def rb( bs : immutable.Seq[Byte] ) : immutable.Seq[Byte] = {

        // we pass Ints, because Bytes coerced to Ints are unsigned (as we
        // expect and require, and because the JVM'll promote them to Ints
        // anyway during various operations.

        def _rb( bytes : immutable.Seq[Int] ) : immutable.Seq[Byte] = {
          lazy val len = bytes.length;
          bytes match {
            case immutable.Seq( b ) if ( b < 128 )  => immutable.Seq( b.toByte );
            case _                  if ( len < 56 ) => (((128 + len) +: bytes) : immutable.Seq[Int]).map( _.toByte );
            case _                                  => {
              val lenBytes = be( len );
              (183 + lenBytes.length).toByte +: (lenBytes ++: bytes.map(  _.toByte ))
            }
          }
        }
        def _promote( bytes : immutable.Seq[Byte] ) : immutable.Seq[Int] = bytes.map( _ & 0xFF );

        _rb( _promote( bs ) );
      }
      def rl( encodables : immutable.Seq[Encodable] ) : immutable.Seq[Byte] = {

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
      def s( encodables : immutable.Seq[Encodable] ) : immutable.Seq[Byte] = encodables.flatMap( encode _ );

      encodable match {
        case bse : Encodable.ByteSeq        => rb( bse.bytes );
        case ie  : Encodable.UnsignedInt    => rba( be( ie.value ) );
        case bie : Encodable.UnsignedBigInt => rba( be( bie.value ) );
        case ese : Encodable.Seq            => rl( ese.seq );
      }
    }
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
        if ( obj.isInstanceOf[scala.Seq[_]] ) {
          val seq : scala.Seq[_] = obj.asInstanceOf[scala.Seq[_]];

          def tryAsHomogenousByteSeq : Option[Encodable] = {
            seq match {
              case scala.Seq() => Some( Encodable.Seq( scala.Seq.empty[Encodable] ) );
              case scala.Seq( head, tail @ _* ) => {
                head match {
                  case byte : Byte => {
                    if ( tail.forall( _.isInstanceOf[Byte] ) ) { // yay! scala.Seq[Byte]
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
    private[this] def be( i : Int )      = IntegerUtils.byteArrayFromInt( i ).dropWhile( _ == 0 );
    private[this] def be( bi : BigInt )  = bi.toByteArray.dropWhile( _ == 0 );

    // Note: We're relying on the fact that an uninitialized byte is guaranteed by the JVM to be zero. 
    private[RLP] def unbeInt( truncatedIntBytes : scala.Seq[Byte] ) = IntegerUtils.intFromByteArray( Array.ofDim[Byte](8 - truncatedIntBytes.length) ++ truncatedIntBytes, 0 ); 
  }
  sealed trait Encodable {
    def isSimple : Boolean;
    def simplify : Encodable.Basic;
  }
}

