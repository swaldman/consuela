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

    def toRLPElement( rlpSerializable : T )               : RLP.Element = serializer.toRLPElement( rlpSerializable );
    def fromRLPElement( element : RLP.Element.Basic ) : Failable[T]   = serializer.fromRLPElement( element );

    def decodeRLP( bytes : Seq[Byte] )         : ( Failable[T], Seq[Byte] ) = serializer.decodeRLP( bytes );
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T]                = serializer.decodeCompleteRLP( bytes );
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte]              = serializer.encodeRLP( rlpSerializable );
  }
  abstract class AbstractWrapper[T : RLPSerializing] extends Wrapper[T] {
    val serializer : RLPSerializing[T] = implicitly[RLPSerializing[T]];
  }
  class ByteArrayValue[T <: util.ByteArrayValue]( factory : immutable.Seq[Byte] => T ) extends RLPSerializing[T] {
    def toRLPElement( t : T )                             : RLP.Element = RLP.Element.ByteSeq( t.bytes );
    def fromRLPElement( element : RLP.Element.Basic ) : Failable[T] = {
      element match {
        case RLP.Element.ByteSeq( bytes ) => Try( factory( bytes ) ).toFailable;
        case _                              => failNotLeaf( element );
      }
    }
  }
  class HomogeneousElementSeq[U : RLPSerializing] extends RLPSerializing[immutable.Seq[U]] {
    def toRLPElement( seq : immutable.Seq[U] ) : RLP.Element = RLP.Element.Seq( asElements( seq ) );
    def fromRLPElement( element : RLP.Element.Basic ) : Failable[immutable.Seq[U]] = {
      element match {
        case RLP.Element.Seq( elements ) => {
          val rlpSerializing = implicitly[RLPSerializing[U]];
          elements.foldLeft( succeed( immutable.Seq.empty[U] ) ){ ( failable : Failable[immutable.Seq[U]], element : RLP.Element ) =>
            failable match {
              case Left(_) => failable;
              case Right( nascentSeq ) => {
                val mbDecoded : Failable[U] = rlpSerializing.fromRLPElement( element.simplify );
                mbDecoded match {
                  case ouch : Left[Fail,U]  => refail( ouch );
                  case good : Right[Fail,U] => Right( nascentSeq :+ good.get );
                }
              }
            }
          }
        }
        case _ => failNotSeq( element );
      }
    }
  }

  // really useful to keep RLPSerializing instances concise
  import scala.language.implicitConversions
  implicit def asElement[ U : RLPSerializing ]( u : U ) : RLP.Element = implicitly[RLPSerializing[U]].toRLPElement( u )

  //not implicit, use this explicitly
  def asElements[U : RLPSerializing]( seq : immutable.Seq[U] ) : immutable.Seq[RLP.Element] = seq.map( u => asElement( u ) )
}
abstract class RLPSerializing[T] {
  // extend and override these two methods. that's it!
  def toRLPElement( rlpSerializable : T )               : RLP.Element;

  /**
   *  The element must be simplified, ie only Element.Seq and Element.ByteSeq entities.
   */
  def fromRLPElement( element : RLP.Element.Basic ) : Failable[T];

  def decodeRLP( bytes : Seq[Byte] ) : ( Failable[T], Seq[Byte] ) = {
    val (element, rest) = RLP.Element.decode( bytes );
    ( fromRLPElement( element.simplify ), rest )
  }
  def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T] = {
    val ( mbDecoded, rest ) = decodeRLP( bytes );
    if (mbDecoded.isRight && rest.length > 0 ) {
      fail(s"RLPSerializing ${this.getClass.getName} decodeCompleteRLP(...) received bytes for ${mbDecoded} with 0x${rest.hex} left over.");
    } else {
      mbDecoded
    }
  }
  def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.Element.encode( toRLPElement( rlpSerializable ) );

  protected def failNotLeaf( found : Any ) : Failable[Nothing] = {
    fail(s"Expected a RLP.Element.ByteSeq( bytes ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
  protected def failNotSeq( found : Any ) : Failable[Nothing] = {
    fail(s"Expected a RLP.Element.Seq( element ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
}
