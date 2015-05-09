package com.mchange.sc.v1.consuela.hash;

import scala.collection._;

import com.mchange.sc.v1.consuela._;

object SHA3_256 extends Hasher.Abstract[SHA3_256] with Hasher.FixedLength[SHA3_256] {
  val AlgoName = "SHA3-256";
  val HashLength = 32;

  protected def instantiate( bytes : Array[Byte] ) : SHA3_256 = new SHA3_256( bytes );
}
final class SHA3_256 private( protected val _bytes : Array[Byte] ) extends Hash[SHA3_256] {
  val hasher = SHA3_256;
}
