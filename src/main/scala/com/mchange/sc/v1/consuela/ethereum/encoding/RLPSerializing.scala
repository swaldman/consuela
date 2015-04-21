package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.util;

import scala.collection._;

import com.mchange.sc.v1.log._;
import MLevel._;

import scala.reflect.ClassTag;

object RLPSerializing {
  /**
   *  If you want to hang RLP decode/encode methods off of a companion object, have it 
   *  implement Wrapper[T], where T is the type to de encoded/decode.
   */ 
  trait Wrapper[T] {
    val serializer : RLPSerializing[T];

    def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable = serializer.toRLPEncodable( rlpSerializable );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T]     = serializer.fromRLPEncodable( encodable );

    def decodeRLP( bytes : Seq[Byte] )         : ( Option[T], Seq[Byte] ) = serializer.decodeRLP( bytes );
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Option[T]                = serializer.decodeCompleteRLP( bytes );
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte]            = serializer.encodeRLP( rlpSerializable );
  }
  abstract class AbstractWrapper[T : RLPSerializing] extends Wrapper[T] {
    val serializer : RLPSerializing[T] = implicitly[RLPSerializing[T]];
  }
  class ByteArrayValue[T <: util.ByteArrayValue]( factory : immutable.Seq[Byte] => T )( implicit evidence : ClassTag[T] ) extends RLPSerializing[T]()( evidence ) {
    def toRLPEncodable( t : T )                             : RLP.Encodable = RLP.Encodable.ByteSeq( t.bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T] = {
      encodable match {
        case RLP.Encodable.ByteSeq( bytes ) => Some( factory( bytes ) )
        case _                              => None
      }
    }
  }
}
abstract class RLPSerializing[T : ClassTag] {
  implicit val logger = MLogger( this );

  // extend and override these two methods. that's it!
  def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable;
  def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T];

  def decodeRLP( bytes : Seq[Byte] ) : ( Option[T], Seq[Byte] ) = {
    val (encodable, rest) = RLP.Encodable.decode( bytes );
    fromRLPEncodable( encodable.simplify ) match {
      case Some( t ) => ( Some(t) , rest )
      case None      => {
        WARNING.log( s"Could not parse encodable ${encodable} to ${SimpleName}" );
        ( None, rest )
      }
    }
  }
  def decodeCompleteRLP( bytes : Seq[Byte] ) : Option[T] = {
    val ( mbSerializable, rest ) = decodeRLP( bytes );
    if ( rest.length > 0 ) {
      throw new IllegalArgumentException(
        s"RLPSerializing decodeCompleteRLP(...) expects exactly the bytes of a ${SimpleName}; received bytes for ${mbSerializable} with 0x${rest.hex} left over."
      )
    } else {
      mbSerializable
    }
  }
  def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.Encodable.encode( toRLPEncodable( rlpSerializable ) );

  private lazy val SimpleName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
}
