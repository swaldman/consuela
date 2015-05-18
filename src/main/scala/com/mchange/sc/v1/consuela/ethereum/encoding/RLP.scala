package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela._

import scala.util.Try;
import java.nio.charset.{Charset, StandardCharsets};
import com.mchange.lang.{ShortUtils, IntegerUtils, LongUtils};

import scala.collection._;
import scala.reflect.ClassTag;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

object RLP {

  def toElement[T : RLPSerializing]( t : T )                         : RLP.Element = implicitly[RLPSerializing[T]].toElement( t );
  def fromElement[T : RLPSerializing]( element : RLP.Element.Basic ) : Failable[T] = implicitly[RLPSerializing[T]].fromElement( element );

  def decode[T : RLPSerializing]( bytes : Seq[Byte] )                    : ( Failable[T], Seq[Byte] ) = implicitly[RLPSerializing[T]].decode( bytes );
  def decodeComplete[T : RLPSerializing]( bytes : Seq[Byte] )            : Failable[T]                = implicitly[RLPSerializing[T]].decodeComplete( bytes ); 
  def encode[T : RLPSerializing]( t : T )                                : immutable.Seq[Byte]        = implicitly[RLPSerializing[T]].encode( t ); 

  // convenience methods
  def encodeString( str : String, charset : Charset ) : immutable.Seq[Byte] = Element.encode( Element.ByteSeq( str.getBytes( charset ).toSeq ) );

  final object Encoded {
    val EmptyByteSeq = Element.encode( Element.EmptyByteSeq );
    val EmptySeq     = Element.encode( Element.EmptySeq );
  }
  final object UTF_8 {
    val _Charset = StandardCharsets.UTF_8;

    /**
     * Strings interpreted as UTF-8 bytes. Strings can be empty, other empty sequences interpreted as structure
     */  
    def fastEncode( obj : Any ) : Option[Seq[Byte]] = Element.fastElementWithStrings( obj, _Charset ).map( Element.encode _ )

    def encodeString( str : String ) : Seq[Byte] = RLP.encodeString( str, _Charset )
  }

  /**
   * Elements are now formally immutable.
   */ 
  final object Element {

    sealed trait Basic extends Element;

    def sameBytes( a : Element, b : Element ) = a.simplify == b.simplify

    final object ByteSeq {
      def apply( array : Array[Byte] )   : ByteSeq = new ByteSeq( ImmutableArraySeq.Byte( array ) );
      def apply( seq : scala.Seq[Byte] ) : ByteSeq = new ByteSeq( toImmutableBytes( seq ) );
    }
    final case class ByteSeq( bytes : immutable.Seq[Byte] ) extends Element.Basic {
      def isSimple = true;
      def simplify = this;
    }
    final case class UnsignedByte( value : scala.Byte ) extends Element {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Element.ByteSeq( scalarBytes( value ) )
    }
    final case class UnsignedShort( value : scala.Short ) extends Element {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Element.ByteSeq( scalarBytes( value ) )
    }
    final case class UnsignedInt( value : scala.Int ) extends Element {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Element.ByteSeq( scalarBytes( value ) )
    }
    final case class UnsignedLong( value : scala.Long ) extends Element {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Element.ByteSeq( scalarBytes( value ) )
    }
    final case class UnsignedBigInt( value : scala.BigInt ) extends Element {
      require( value >= 0 );

      def isSimple = false;
      def simplify = Element.ByteSeq( scalarBytes( value ) )
    }
    final object Seq {
      final object of {
        def apply( elements : Element* )  : Element.Seq = new Seq( ImmutableArraySeq( elements.toArray ) );
        def unapplySeq( seq : Element.Seq ) : Option[immutable.Seq[Element]] = Some(seq.seq);
      }
      def apply( array : Array[Element] )   = new Seq( ImmutableArraySeq( array ) );
      def apply( seq : scala.Seq[Element] ) = new Seq( toImmutable( seq ) );
    }
    final case class Seq( seq : immutable.Seq[Element] ) extends Element.Basic {
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

    def decodeComplete( bytes : scala.Seq[Byte] ) : Element.Basic = decode( bytes ).ensuring( _._2.isEmpty )._1

    /**
     *  @return a pair, decoded Element and unconsumed bytes
     */  
    def decode( bytes : scala.Seq[Byte] ) : (Element.Basic, scala.Seq[Byte]) = {
      def splitOut( splitMe : scala.Seq[Byte], len : Int ) = {
        val ( decoded, rest ) = splitMe.splitAt( len );
        ( Element.ByteSeq( decoded ), rest )
      }
      def splitOutSeq( splitMe : scala.Seq[Byte], len : Int ) = {
        val (elements, rest) = elementSeq( len, Nil, splitMe );
        ( Element.Seq( elements ), rest )
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
      def elementSeq( count : Int, reverseAccum : List[Element.Basic], in : scala.Seq[Byte] ) : ( scala.Seq[Element.Basic], scala.Seq[Byte] ) = {
        assert(
          count >= 0,
          s"Count should begin with precisely the byte length of the concatenated elements, and so we should end with precisely zero bytes. count -> ${count}, reverseAccum -> ${reverseAccum}"
        );

        if ( count == 0 )
          ( reverseAccum.reverse, in );
        else {
          val ( element, rest ) = decode( in );
          val eaten = in.length - rest.length
          elementSeq( count - eaten, element :: reverseAccum, rest );
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
    def encode( element : Element ) : scala.collection.immutable.Seq[Byte] = {

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
              val lenBytes = scalarBytes( len );
              (183 + lenBytes.length).toByte +: (lenBytes ++: bytes.map(  _.toByte ))
            }
          }
        }
        def _promote( bytes : immutable.Seq[Byte] ) : immutable.Seq[Int] = bytes.map( _ & 0xFF );

        _rb( _promote( bs ) );
      }
      def rl( elements : immutable.Seq[Element] ) : immutable.Seq[Byte] = {

        // note: this was a surpise to me. the length of s(x), described in the ethereum yellow paper as ||s(x)|| is
        // the *byte length of the serialized representation of all concatenated members of the sequence*,
        // not the length of the top-level sequence, i.e. not the number of members the will be concatenated into
        // the sequence.

        val selements = s( elements );
        val len = selements.length;
        if ( len < 56 )
          (192 + len).toByte +: selements
        else {
          val lenBytes = scalarBytes( len );
          (247 + lenBytes.length).toByte +: ( lenBytes ++: selements )
        }
      }
      def s( elements : immutable.Seq[Element] ) : immutable.Seq[Byte] = elements.flatMap( encode _ );

      element match {
        case bse : Element.ByteSeq        => rb( bse.bytes );
        case be  : Element.UnsignedByte   => rba( scalarBytes( be.value ) );
        case se  : Element.UnsignedShort  => rba( scalarBytes( se.value ) );
        case ie  : Element.UnsignedInt    => rba( scalarBytes( ie.value ) );
        case le  : Element.UnsignedLong   => rba( scalarBytes( le.value ) );
        case bie : Element.UnsignedBigInt => rba( scalarBytes( bie.value ) );
        case ese : Element.Seq            => rl( ese.seq );
      }
    }
    /**
     * Strings can be empty, other empty sequences interpreted as structure
     */  
    def fastElementWithStrings( obj : Any, charset : Charset ) : Option[Element] = {
      def tryAsAtom : Option[Element] = {
        obj match {
          case b   : Byte   if (b >= 0)  => Some( Element.UnsignedByte( b ) )
          case s   : Short  if (s >= 0)  => Some( Element.UnsignedShort( s ) )
          case i   : Int    if (i >= 0)  => Some( Element.UnsignedInt( i ) )
          case l   : Long   if (l >= 0)  => Some( Element.UnsignedLong( l ) )
          case bi  : BigInt if (bi >= 0) => Some( Element.UnsignedBigInt( bi ) )
          case str : String              => Some( Element.ByteSeq( str.getBytes( charset ) ) )
          case _                         => None
        }
      }
      def tryAsSeq : Option[Element] = {
        if ( obj.isInstanceOf[scala.Seq[_]] ) {
          val seq : scala.Seq[_] = obj.asInstanceOf[scala.Seq[_]];

          def tryAsHomogenousByteSeq : Option[Element] = {
            seq match {
              case scala.Seq() => Some( Element.Seq( scala.Seq.empty[Element] ) );
              case scala.Seq( head, tail @ _* ) => {
                head match {
                  case byte : Byte => {
                    if ( tail.forall( _.isInstanceOf[Byte] ) ) { // yay! scala.Seq[Byte]
                      Some( Element.ByteSeq( byte +: tail.map( _.asInstanceOf[Byte] ) ) )
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
          def tryAsOtherSeq : Option[Element] = Try( Element.Seq( seq.map( fastElementWithStrings( _, charset ).get ) ) ).toOption

          tryAsHomogenousByteSeq orElse tryAsOtherSeq
        } else {
          None
        }
      }

      tryAsAtom orElse tryAsSeq
    }
    def scalarBytes( b  : Byte )   : Array[Byte] = Array[Byte](b).dropWhile( _ == 0 );
    def scalarBytes( s  : Short )  : Array[Byte] = ShortUtils.byteArrayFromShort( s ).dropWhile( _ == 0 );
    def scalarBytes( i  : Int )    : Array[Byte] = IntegerUtils.byteArrayFromInt( i ).dropWhile( _ == 0 );
    def scalarBytes( l  : Long )   : Array[Byte] = LongUtils.byteArrayFromLong( l ).dropWhile( _ == 0 );
    def scalarBytes( bi : BigInt ) : Array[Byte] = bi.toByteArray.dropWhile( _ == 0 );

    val Byte0 = 0.toByte;

    // Note: We're relying on the fact that an uninitialized byte is guaranteed by the JVM to be zero. 
    def byteFromScalarBytes( truncatedByte : scala.Seq[Byte] ) : Byte = {
      truncatedByte.length match {
        case 0 => Byte0;
        case 1 => truncatedByte(0);
        case _ => throw new IllegalArgumentException( s"Expected a Seq containing a single Byte, found ${truncatedByte}" );
      }
    }
    def shortFromScalarBytes( truncatedShortBytes : scala.Seq[Byte] ) : Short = {
      ShortUtils.shortFromByteArray( Array.ofDim[Byte](2 - truncatedShortBytes.length) ++ truncatedShortBytes, 0 )
    }
    def intFromScalarBytes( truncatedIntBytes : scala.Seq[Byte] ) : Int = {
      IntegerUtils.intFromByteArray( Array.ofDim[Byte](4 - truncatedIntBytes.length) ++ truncatedIntBytes, 0 )
    }
    def longFromScalarBytes( truncatedLongBytes : scala.Seq[Byte] ) : Long = {
      LongUtils.longFromByteArray( Array.ofDim[Byte](8 - truncatedLongBytes.length) ++ truncatedLongBytes, 0 )
    }
  }
  sealed trait Element {
    def isSimple : Boolean;
    def simplify : Element.Basic;
  }
}

