package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

object EthSignature {

  // signature constraints taken from the ethereum yellow paper (current github version, 2015-04-10)
  val PossibleVs = Set( 27.toByte, 28.toByte );
  val MAX_EXCLUSIVE_r = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);
  val MAX_EXCLUSIVE_s = {
    val TWO = BigInt(2);
    (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977)
  }

  trait Exact {
    self : EthSignature =>

    val v : Byte;
  }
}
sealed trait EthSignature {
  val r : BigInt;
  val s : BigInt;

  protected lazy val tailByteSeq = Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* );

  protected def validRS = this.r < EthSignature.MAX_EXCLUSIVE_r && this.s < EthSignature.MAX_EXCLUSIVE_s;
}
case class ApproximateEthSignature( val r : BigInt, val s : BigInt ) extends EthSignature {
  require( validRS );

  lazy val exportByteSeqs = EthSignature.PossibleVs.map( _ +: tailByteSeq );
}
case class ExactEthSignature( val v : Byte, val r : BigInt, val s : BigInt ) extends EthSignature with EthSignature.Exact {
  require( validRS && EthSignature.PossibleVs(v) );

  lazy val exportByteSeq = v.toByte +: tailByteSeq;
}
