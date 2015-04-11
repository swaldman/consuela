package com.mchange.sc.v1.consuela.hash;

import scala.util.hashing.MurmurHash3;

import com.mchange.sc.v1.consuela.Implicits._;

object Hash {
  object SHA3_256 extends Hasher[SHA3_256] with Hasher.FixedLength {
    def withBytes( bytes : Array[Byte] ) : SHA3_256 = {
      require( bytes.length == HashLength, s"An SHA3_356 hash must have a fixed length of ${HashLength} bytes, cannot create with ${bytes.length} bytes. bytes -> ${bytes}" )
      new SHA3_256(bytes);
    }
    def withBytes( bytes : Seq[Byte] ) : SHA3_256 = withBytes( bytes.toArray );
    def hash( bytes : Array[Byte] ) : SHA3_256 = new SHA3_256(hash_SHA3_256(bytes));
    def hash( bytes : Seq[Byte] ) : SHA3_256 = this.hash( bytes.toArray );
    val Zero = new SHA3_256( Array.fill[Byte](32)(0) );
    val HashLength = 32;
  }
  class SHA3_256 private( protected[this] val _bytes : Array[Byte] ) extends Hash[SHA3_256] {
    //protected[this] def sameClass( other : Any ) : Boolean = other.isInstanceOf[SHA3_256];
    protected[this] val stringTag = "SHA3_256"
    val hasher = SHA3_256;
  }
}
trait Hash[T <: Hash[T]] {
  protected def _bytes : Array[Byte];
  protected[this] def sameClass( other : Any ) : Boolean = this.getClass == other.getClass;
  protected[this] def stringTag : String;

  def hasher : Hasher[T];

  lazy val bytes : IndexedSeq[Byte] = IndexedSeq( _bytes : _* );

  def toByteArray : Array[Byte] = _bytes.clone();
  def toBigInt : BigInt = BigInt( _bytes );
  def hex = _bytes.hex

  lazy val hexbytes = com.mchange.lang.ByteUtils.toLowercaseHexAscii( bytes.toArray );

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
