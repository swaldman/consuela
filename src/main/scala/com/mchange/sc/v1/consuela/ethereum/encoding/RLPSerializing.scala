/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.util;

import com.mchange.sc.v3.failable._;

import scala.collection._

import scala.util.Try;

import scala.reflect.ClassTag;

object RLPSerializing {

  /**
   *  If you want to hang RLP decode/encode methods off of a companion object, have it 
   *  implement Wrapper[T], where T is the type to de encoded/decode.
   */ 
  trait Wrapper[T] {
    val serializer : RLPSerializing[T];

    def toRLPElement( rlpSerializable : T )           : RLP.Element = serializer.toElement( rlpSerializable );
    def fromRLPElement( element : RLP.Element.Basic ) : Failable[T]   = serializer.fromElement( element );

    def decodeRLP( bytes : Seq[Byte] )         : ( Failable[T], Seq[Byte] ) = serializer.decode( bytes );
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Failable[T]                = serializer.decodeComplete( bytes );
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte]              = serializer.encode( rlpSerializable );
  }
  abstract class AbstractWrapper[T : RLPSerializing] extends Wrapper[T] {
    val serializer : RLPSerializing[T] = implicitly[RLPSerializing[T]];
  }
  class ByteArrayValue[T <: util.ByteArrayValue]( factory : immutable.Seq[Byte] => T ) extends RLPSerializing[T] {
    def toElement( t : T )                         : RLP.Element = RLP.Element.ByteSeq( t.bytes );
    def fromElement( element : RLP.Element.Basic ) : Failable[T] = {
      element match {
        case RLP.Element.ByteSeq( bytes ) => Try( factory( bytes ) ).toFailable;
        case _                            => failNotLeaf( element );
      }
    }
  }
  class HomogeneousElementSeq[U : RLPSerializing] extends RLPSerializing[immutable.Seq[U]] {
    def toElement( seq : immutable.Seq[U] ) : RLP.Element = RLP.Element.Seq( asElements( seq ) );
    def fromElement( element : RLP.Element.Basic ) : Failable[immutable.Seq[U]] = {
      element match {
        case RLP.Element.Seq( elements ) => {
          val rlpSerializing = implicitly[RLPSerializing[U]];
          elements.foldLeft( Failable.succeed( immutable.Seq.empty[U] ) ){ ( failable : Failable[immutable.Seq[U]], element : RLP.Element ) =>
            failable match {
              case _ : Failed[_] => failable;
              case Succeeded( nascentSeq ) => {
                val mbDecoded : Failable[U] = rlpSerializing.fromElement( element.simplify );
                mbDecoded match {
                  case ouch : Failed[U]  => Failable.refail( ouch );
                  case good : Succeeded[U] => Succeeded( nascentSeq :+ good.get );
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
  implicit def asElement[ U : RLPSerializing ]( u : U ) : RLP.Element = implicitly[RLPSerializing[U]].toElement( u )

  //not implicit, use this explicitly
  def asElements[U : RLPSerializing]( seq : immutable.Seq[U] ) : immutable.Seq[RLP.Element] = seq.map( u => asElement( u ) )
}
trait RLPSerializing[T] {
  // extend and override these two methods. that's it!
  def toElement( rlpSerializable : T )               : RLP.Element;

  /**
   *  The element must be simplified, ie only Element.Seq and Element.ByteSeq entities.
   */
  def fromElement( element : RLP.Element.Basic ) : Failable[T];

  def decode( bytes : Seq[Byte] ) : ( Failable[T], Seq[Byte] ) = {
    val (element, rest) = RLP.Element.decode( bytes );
    ( fromElement( element.simplify ), rest )
  }
  def decodeComplete( bytes : Seq[Byte] ) : Failable[T] = {
    val ( mbDecoded, rest ) = decode( bytes );
    if (mbDecoded.isSucceeded && rest.length > 0 ) {
      Failable.fail(s"RLPSerializing ${this.getClass.getName} decodeComplete(...) received bytes for ${mbDecoded} with 0x${rest.hex} left over.");
    } else {
      mbDecoded
    }
  }
  def encode( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.Element.encode( toElement( rlpSerializable ) );

  protected def failNotLeaf( found : Any ) : Failable[Nothing] = {
    Failable.fail(s"Expected a RLP.Element.ByteSeq( bytes ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
  protected def failNotSeq( found : Any ) : Failable[Nothing] = {
    Failable.fail(s"Expected a RLP.Element.Seq( element ) when deserializing with ${this.getClass.getName}, found ${found}.")
  }
}
