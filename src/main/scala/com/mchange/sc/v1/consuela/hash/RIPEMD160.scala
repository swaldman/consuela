package com.mchange.sc.v1.consuela.hash;

import scala.collection._;

object RIPEMD160 extends Hasher.Jca[RIPEMD160] with Hasher.FixedLength[RIPEMD160] {
  val AlgoName = "RIPEMD160";
  val HashLength = 20;

  protected def instantiate( bytes : Array[Byte] ) : RIPEMD160 = new RIPEMD160( bytes );
}
final class RIPEMD160 private( protected val _bytes : Array[Byte] ) extends Hash[RIPEMD160] {
  val hasher = RIPEMD160;
}

