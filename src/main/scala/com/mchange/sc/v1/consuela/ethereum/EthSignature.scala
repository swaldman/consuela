package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

import specification.Set.SignatureV;
import specification.Set.SignatureR;
import specification.Set.SignatureS;

object EthSignature {

  val PossibleVs = Set( 27.toByte, 28.toByte );
  val MAX_EXCLUSIVE_r = SignatureR.MaxValueExclusive;
  val MAX_EXCLUSIVE_s = SignatureS.MaxValueExclusive;

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
