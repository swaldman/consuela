package com.mchange.sc.v1.consuela.hash;

import scala.collection._;

object SHA256 extends Hasher.Jca[SHA256] with Hasher.FixedLength[SHA256] {
  val AlgoName = "SHA256";
  val HashLength = 32;

  protected def instantiate( bytes : Array[Byte] ) : SHA256 = new SHA256( bytes );
}
final class SHA256 private( protected val _bytes : Array[Byte] ) extends Hash[SHA256] {
  val hasher = SHA256;
}

