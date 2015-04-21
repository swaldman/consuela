package com.mchange.sc.v1.consuela.hash;

import scala.collection._;

import com.mchange.sc.v1.consuela._;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

object Hash {
  object SHA3_256 extends Hasher[SHA3_256] with Hasher.FixedLength {
    def withBytes( bytes : Array[Byte] ) : SHA3_256 = {
      require( bytes.length == HashLength, s"An SHA3_356 hash must have a fixed length of ${HashLength} bytes, cannot create with ${bytes.length} bytes. bytes -> ${bytes}" )
      new SHA3_256(bytes.clone());
    }
    def withBytes( bytes : Seq[Byte] ) : SHA3_256 = withBytes( bytes.toArray );
    def hash( bytes : Array[Byte] ) : SHA3_256 = new SHA3_256(hash_SHA3_256(bytes));
    def hash( bytes : Seq[Byte] ) : SHA3_256 = this.hash( bytes.toArray );
    val Zero = new SHA3_256( Array.fill[Byte](32)(0) );
    val HashLength = 32;
  }
  final class SHA3_256 private( protected val _bytes : Array[Byte] ) extends Hash[SHA3_256] {
    val hasher = SHA3_256;
  }
}
trait Hash[T <: Hash[T]] extends ByteArrayValue with ByteArrayValue.UnsignedBigIntegral {
  def hasher : Hasher[T];
}
