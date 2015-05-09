package com.mchange.sc.v1.consuela.hash;

import scala.collection._;

import com.mchange.sc.v1.consuela._;

object SHA3_512 extends Hasher.Abstract[SHA3_512] with Hasher.FixedLength[SHA3_512] {
  val AlgoName = "SHA3-512";
  val HashLength = 64;

  protected def instantiate( bytes : Array[Byte] ) : SHA3_512 = new SHA3_512( bytes );
}
final class SHA3_512 private( protected val _bytes : Array[Byte] ) extends Hash[SHA3_512] {
  val hasher = SHA3_512;
}
