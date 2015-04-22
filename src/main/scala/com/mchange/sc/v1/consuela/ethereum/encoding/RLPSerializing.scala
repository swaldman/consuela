package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.util;

import scala.collection._;

import scala.util.Try;

import scala.reflect.ClassTag;

object RLPSerializing {
  /**
   *  If you want to hang RLP decode/encode methods off of a companion object, have it 
   *  implement Wrapper[T], where T is the type to de encoded/decode.
   */ 
  trait Wrapper[T] {
    val serializer : RLPSerializing[T];

    def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable = serializer.toRLPEncodable( rlpSerializable );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[T]   = serializer.fromRLPEncodable( encodable );

    def decodeRLP( bytes : Seq[Byte] )         : ( Failable[T], Seq[Byte] ) = serializer.decodeRLP( bytes );
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T]                = serializer.decodeCompleteRLP( bytes );
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte]              = serializer.encodeRLP( rlpSerializable );
  }
  abstract class AbstractWrapper[T : RLPSerializing] extends Wrapper[T] {
    val serializer : RLPSerializing[T] = implicitly[RLPSerializing[T]];
  }
  class ByteArrayValue[T <: util.ByteArrayValue]( factory : immutable.Seq[Byte] => T )( implicit evidence : ClassTag[T] ) extends RLPSerializing[T]()( evidence ) {
    def toRLPEncodable( t : T )                             : RLP.Encodable = RLP.Encodable.ByteSeq( t.bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[T] = {
      encodable match {
        case RLP.Encodable.ByteSeq( bytes ) => Try( factory( bytes ) ).toFailable;
        case _                              => failNotLeaf( encodable );
      }
    }
  }
}
abstract class RLPSerializing[T : ClassTag] {

  // extend and override these two methods. that's it!
  def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable;
  def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[T];

  def decodeRLP( bytes : Seq[Byte] ) : ( Failable[T], Seq[Byte] ) = {
    val (encodable, rest) = RLP.Encodable.decode( bytes );
    ( fromRLPEncodable( encodable.simplify ), rest )
  }
  def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T] = {
    val ( mbDecoded, rest ) = decodeRLP( bytes );
    if (mbDecoded.isRight && rest.length > 0 ) {
      fail(s"RLPSerializing decodeCompleteRLP(...) expects exactly the bytes of a ${TargetClassName}; received bytes for ${mbDecoded} with 0x${rest.hex} left over.");
    } else {
      mbDecoded
    }
  }
  def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.Encodable.encode( toRLPEncodable( rlpSerializable ) );

  protected lazy val TargetClassName = implicitly[ClassTag[T]].runtimeClass.getSimpleName

  protected def failNotLeaf( found : Any ) : Failable[Nothing] = fail(s"Expected a RLP.Encodable.ByteSeq( bytes ) when deserializing ${TargetClassName}, found ${found}.")
}
