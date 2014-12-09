package com.mchange.sc.v1.consuela.hash;

import scala.util.hashing.MurmurHash3;

import com.mchange.sc.v1.consuela.conf.Config.Implicits._;

object Hash {
  object SHA3_256 {
    def apply( _bytes : Array[Byte] ) : SHA3_256 = new SHA3_256(hash_SHA3_256(_bytes));
    val Zero = new SHA3_256( Array.fill[Byte](32)(0) );
  }
  class SHA3_256 private( protected[this] val _bytes : Array[Byte] ) extends Hash[SHA3_256] {
    protected[this] def sameClass( other : Any ) : Boolean = other.isInstanceOf[SHA3_256];
    protected[this] val stringTag = "SHA3_256"
  }
}
trait Hash[T <: Hash[T]] {
  protected def _bytes : Array[Byte];
  protected[this] def sameClass( other : Any ) : Boolean;
  protected[this] def stringTag : String;

  lazy val bytes : IndexedSeq[Byte] = IndexedSeq( _bytes : _* );

  def toByteArray : Array[Byte] = _bytes.clone();
  def toBigInt : BigInt = BigInt( _bytes );

  lazy val hexbytes = com.mchange.lang.ByteUtils.toHexAscii( bytes.toArray ).toLowerCase; //ok. we should add a lowercase switch to ByteUtils. but for now...

  override def equals( other : Any ) : Boolean = {
    if (! sameClass( other ))
      false
    else {
      val o = other.asInstanceOf[Hash[T]];
      this.bytes.length == o.bytes.length && (0 until this.bytes.length).forall( i => ((this.bytes(i) ^ o.bytes(i)) == 0) )
    }
  }
  override def hashCode : Int = MurmurHash3.bytesHash( bytes.toArray );

  override def toString : String = s"${stringTag}[${this.hexbytes}]"
}
