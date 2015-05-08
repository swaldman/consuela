package com.mchange.sc.v1.consuela.util;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import java.math.BigInteger;

import scala.collection.immutable.IndexedSeq;

object ByteSeqValue {
  trait BigIntegral {
    self : ByteSeqValue =>

    protected def buildBigInteger = new BigInteger( self.toByteArray );

    lazy val toBigInteger : BigInteger  = buildBigInteger;
    lazy val toBigInt     : BigInt      = BigInt( toBigInteger );
  }
  trait UnsignedBigIntegral extends BigIntegral {
    self : ByteSeqValue =>

    protected override def buildBigInteger = new BigInteger( 1, self.toByteArray );
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

