package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;


object EthSignature {
  val PossibleVs = Set( 27.toByte, 28.toByte );

  trait Exact {
    self : EthSignature =>

    def v : Byte;
  }
}
sealed trait EthSignature {
  def r : BigInt;
  def s : BigInt;

  protected lazy val tailByteSeq = Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* );
}
case class ApproximateEthSignature( val r : BigInt, val s : BigInt ) extends EthSignature {
  lazy val exportByteSeqs = EthSignature.PossibleVs.map( _ +: tailByteSeq );
}
case class ExactEthSignature( val v : Byte, val r : BigInt, val s : BigInt ) extends EthSignature with EthSignature.Exact {
  require( EthSignature.PossibleVs(v) );

  lazy val exportByteSeq = v.toByte +: tailByteSeq;
}
