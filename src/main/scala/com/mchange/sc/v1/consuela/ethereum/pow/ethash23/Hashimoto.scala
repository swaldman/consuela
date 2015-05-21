package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import scala.collection._;

import com.mchange.sc.v1.consuela._;
import ethereum.specification.Types.Unsigned256;

case class Hashimoto( val mixDigest : immutable.Seq[Byte], val result : Unsigned256 ) {
  override def toString : String = s"Hashimote(mixDigest=${mixDigest.hex},result=${result.widen.unsignedBytes(32).hex})"
}
