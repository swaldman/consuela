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

package com.mchange.sc.v1.consuela.hash;

import scala.language.reflectiveCalls;

object Hasher {
  trait NamedAlgorithm {
    val AlgoName : String;
  }
  trait FixedLength[T <: Hash[T]] extends Abstract[T] with NamedAlgorithm {
    val HashLength : Int;

    lazy val Zero = instantiate( Array.ofDim[Byte]( HashLength ) );

    private lazy val Ensurer : T => Boolean = _.length == HashLength

    override def withBytes( bytes : Array[Byte], offset : Int, len : Int ) : T = {
      require( len == HashLength, s"A ${AlgoName} hash must have a fixed length of ${HashLength} bytes, cannot create with specified length ${len}." )
      super.withBytes( bytes, offset, len )
    }
    override def withBytes( bytes : Array[Byte] ) : T = {
      require( bytes.length == HashLength, badLenMessage( bytes ) )
      super.withBytes( bytes )
    }
    override def withBytes( bytes : Seq[Byte] ) : T = {
      require( bytes.length == HashLength, badLenMessage( bytes ) )
      super.withBytes( bytes )
    }
    def withBytes( bytes : Array[Byte], offset : Int ) : T = super.withBytes( bytes, offset, HashLength )

    override def hash( bytes : Array[Byte] ) : T = super.hash( bytes ) ensuring( Ensurer, badLenMessage( bytes ) );
    override def hash( bytes : Seq[Byte] ) : T   = super.hash( bytes ) ensuring( Ensurer, badLenMessage( bytes ) );

    private def badLenMessage( bytes : { def length : Int } ) = s"A ${AlgoName} hash must have a fixed length of ${HashLength} bytes, cannot create with ${bytes.length} bytes. bytes -> ${bytes}"
  }
  abstract class Abstract[T <: Hash[T]] extends Hasher[T] {
    protected def instantiate( bytes : Array[Byte] ) : T;

    def withBytes( bytes : Array[Byte], offset : Int, len : Int ) : T = {
      val tmp : Array[Byte] = Array.ofDim[Byte]( len );
      Array.copy( bytes, offset, tmp, 0, len )
      withBytes( tmp )
    }
    def withBytes( bytes : Array[Byte] ) : T = instantiate( bytes.clone() );
    def withBytes( bytes : Seq[Byte] ) : T   = instantiate( bytes.toArray );
    def hash( bytes : Array[Byte] ) : T      = instantiate( rawHash(bytes) );
    def hash( bytes : Seq[Byte] ) : T        = instantiate( rawHash(bytes.toArray) );

    def rawHash( bytes : Array[Byte] ) : Array[Byte];
  }
  abstract class Jca[T <: Hash[T]] extends Hasher.Abstract[T] with NamedAlgorithm {
    def rawHash( bytes : Array[Byte] ) : Array[Byte] = jcaDoHash(AlgoName, bytes)
  }
}

trait Hasher[T <: Hash[T]] {
  def withBytes( bytes : Seq[Byte] ) : T;
  def withBytes( bytes : Array[Byte] ) : T;
  def Zero : T;

  def hash( bytes : Seq[Byte] )   : T;
  def hash( bytes : Array[Byte] ) : T;

  def rawHash( bytes : Array[Byte] ) : Array[Byte];
}
