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

