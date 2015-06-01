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

import java.math.BigInteger;

import scala.collection.immutable.IndexedSeq;

object ByteSeqValue {
  trait BigIntegral {
    self : ByteSeqValue =>

    // they are already lazy vals on ImmutableArraySeq.Byte
    // no reason to cache in two separate references.
    // so defs.
    def toBigInteger : BigInteger  = bytes.asSignedBigInteger;
    def toBigInt     : BigInt      = bytes.asSignedBigInt;
  }
  trait UnsignedBigIntegral extends BigIntegral {
    self : ByteSeqValue =>

    override def toBigInteger : BigInteger  = bytes.asUnsignedBigInteger;
    override def toBigInt     : BigInt      = bytes.asUnsignedBigInt;
  }
}

trait ByteSeqValue {
  val bytes : ImmutableArraySeq.Byte;

  private lazy val classSimpleName = this.getClass.getSimpleName;
  protected def stringTag : String = classSimpleName;
  protected def sameClass( other : Any ) : Boolean = this.getClass == other.getClass;

  lazy val toByteArray : Array[Byte] = bytes.toArray[Byte];
  lazy val hexbytes    : String = bytes.hex

  def hex : String = hexbytes;

  def length : Int = bytes.length;

  override def equals( other : Any ) : Boolean = {
    if (! sameClass( other ))
      false
    else {
      val o = other.asInstanceOf[ByteSeqValue];
      this.bytes == o.bytes
    }
  }

  private lazy val hashCodeValue = bytes.hashCode ^ ByteSeqValue.hashCode;
  override def hashCode : Int = hashCodeValue;

  override def toString : String = s"${stringTag}[${this.hexbytes}]"
}

