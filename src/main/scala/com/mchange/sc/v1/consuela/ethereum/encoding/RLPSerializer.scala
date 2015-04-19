package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

import com.mchange.sc.v1.log._;
import MLevel._;

import scala.reflect.ClassTag;

object RLPSerializer {
  trait Wrapper[T] {
    val serializer : RLPSerializer[T];

    def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable = serializer.toRLPEncodable( rlpSerializable );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T]     = serializer.fromRLPEncodable( encodable );

    def decodeRLP( bytes : Seq[Byte] )         : ( Option[T], Seq[Byte] ) = serializer.decodeRLP( bytes );
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Option[T]                = serializer.decodeCompleteRLP( bytes );
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte]            = serializer.encodeRLP( rlpSerializable );
  }
  abstract class AbstractWrapper[T : RLPSerializer] extends Wrapper[T] {
    val serializer : RLPSerializer[T] = implicitly[RLPSerializer[T]];
  }
}
abstract class RLPSerializer[T : ClassTag] {
  implicit val logger = MLogger( this );

  def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable;
  def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T];

  def decodeRLP( bytes : Seq[Byte] ) : ( Option[T], Seq[Byte] ) = {
    val (encodable, rest) = RLP.decode( bytes );
    fromRLPEncodable( encodable ) match {
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
        s"RLPSerializer decodeCompleteRLP(...) expects exactly the bytes of a ${SimpleName}; received bytes for ${mbSerializable} with 0x${rest.hex} left over."
      )
    } else {
      mbSerializable
    }
  }
  def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.encode( toRLPEncodable( rlpSerializable ) );

  private lazy val SimpleName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
}
