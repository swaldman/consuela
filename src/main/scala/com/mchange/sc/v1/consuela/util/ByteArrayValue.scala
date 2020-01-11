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

package com.mchange.sc.v1.consuela.util;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import scala.collection.immutable.Vector;
import scala.util.hashing.MurmurHash3;

import java.math.BigInteger;
import java.util.Arrays;

import scala.collection.immutable.IndexedSeq;

object ByteArrayValue {
  trait BigIntegral {
    self : ByteArrayValue =>

    protected def buildBigInteger = new BigInteger( this._bytes );

    lazy val toBigInteger : BigInteger  = buildBigInteger;
    lazy val toBigInt     : BigInt      = BigInt( toBigInteger );
  }
  trait UnsignedBigIntegral extends BigIntegral {
    self : ByteArrayValue =>

    protected override def buildBigInteger = new BigInteger( 1, this._bytes );
  }
}

trait ByteArrayValue {
  protected val _bytes : Array[Byte]; // should be protected by a defensive copy if references may have leaked

  private lazy val classSimpleName = this.getClass.getSimpleName;
  protected def stringTag : String = classSimpleName;
  protected def sameClass( other : Any ) : Boolean = this.getClass == other.getClass;

  lazy val bytes : IndexedSeq[Byte] = ImmutableArraySeq.Byte( _bytes );

  lazy val toByteArray : Array[Byte] = _bytes.clone();
  lazy val hexbytes    : String = _bytes.hex

  def hex : String = hexbytes;

  def hex0x = "0x" + hex

  def length : Int = _bytes.length;

  override def equals( other : Any ) : Boolean = {
    if (! sameClass( other ))
      false
    else {
      val o = other.asInstanceOf[ByteArrayValue];
      Arrays.equals( this._bytes, o._bytes );
    }
  }

  private lazy val hashCodeValue = MurmurHash3.bytesHash( _bytes );
  override def hashCode : Int = hashCodeValue;

  override def toString : String = s"${stringTag}[${this.hexbytes}]"
}
