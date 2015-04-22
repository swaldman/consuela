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
  class ByteArrayValue[T <: util.ByteArrayValue]( factory : immutable.Seq[Byte] => T ) extends RLPSerializing[T] {
    def toRLPEncodable( t : T )                             : RLP.Encodable = RLP.Encodable.ByteSeq( t.bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[T] = {
      encodable match {
        case RLP.Encodable.ByteSeq( bytes ) => Try( factory( bytes ) ).toFailable;
        case _                              => failNotLeaf( encodable );
      }
    }
  }
  class HomogeneousEncodableSeq[U : RLPSerializing] extends RLPSerializing[immutable.Seq[U]] {
    def toRLPEncodable( seq : immutable.Seq[U] ) : RLP.Encodable = RLP.Encodable.Seq( seq.map( elem => asEncodable( elem ) ) );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[immutable.Seq[U]] = {
      encodable match {
        case RLP.Encodable.Seq( encodables ) => {
          val rlpSerializing = implicitly[RLPSerializing[U]];
          encodables.foldLeft( succeed( immutable.Seq.empty[U] ) ){ ( failable : Failable[immutable.Seq[U]], encodable : RLP.Encodable ) =>
            failable match {
              case Left(_) => failable;
              case Right( nascentSeq ) => {
                val mbDecoded : Failable[U] = rlpSerializing.fromRLPEncodable( encodable.simplify );
                mbDecoded match {
                  case ouch : Left[Fail,U]  => refail( ouch );
                  case good : Right[Fail,U] => Right( nascentSeq :+ good.get );
                }
              }
            }
          }
        }
        case _ => failNotSeq( encodable );
      }
    }
  }

  // really useful to keep RLPSerializing instances concise
  import scala.language.implicitConversions
  implicit def asEncodable[ U : RLPSerializing ]( u : U ) : RLP.Encodable = implicitly[RLPSerializing[U]].toRLPEncodable( u )
}
abstract class RLPSerializing[T] {
  // extend and override these two methods. that's it!
  def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable;

  /**
   *  The encodable must be simplified, ie only Encodable.Seq and Encodable.ByteSeq entities.
   */
  def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[T];

  def decodeRLP( bytes : Seq[Byte] ) : ( Failable[T], Seq[Byte] ) = {
    val (encodable, rest) = RLP.Encodable.decode( bytes );
    ( fromRLPEncodable( encodable.simplify ), rest )
  }
  def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T] = {
    val ( mbDecoded, rest ) = decodeRLP( bytes );
    if (mbDecoded.isRight && rest.length > 0 ) {
      fail(s"RLPSerializing ${this.getClass.getName} decodeCompleteRLP(...) received bytes for ${mbDecoded} with 0x${rest.hex} left over.");
    } else {
      mbDecoded
    }
  }
  def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.Encodable.encode( toRLPEncodable( rlpSerializable ) );

  protected def failNotLeaf( found : Any ) : Failable[Nothing] = {
    fail(s"Expected a RLP.Encodable.ByteSeq( bytes ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
  protected def failNotSeq( found : Any ) : Failable[Nothing] = {
    fail(s"Expected a RLP.Encodable.Seq( encodable ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
}
