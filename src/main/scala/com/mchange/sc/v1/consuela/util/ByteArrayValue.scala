package com.mchange.sc.v1.consuela.util;

import com.mchange.sc.v1.consuela.Implicits._;

import scala.collection.immutable.Vector;
import scala.util.hashing.MurmurHash3;

import java.math.BigInteger;
import java.util.Arrays;

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
  protected val _bytes : Array[Byte];

  private lazy val classSimpleName = this.getClass.getSimpleName;
  protected def stringTag : String = classSimpleName;
  protected def sameClass( other : Any ) : Boolean = this.getClass == other.getClass;

  lazy val bytes : IndexedSeq[Byte] = Vector( _bytes : _* );

  lazy val toByteArray : Array[Byte] = _bytes.clone();
  lazy val hexbytes    : String = _bytes.hex

  def hex : String = hexbytes;

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
