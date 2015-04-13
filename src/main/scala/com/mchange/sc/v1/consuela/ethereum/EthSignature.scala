package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;


object EthSignature {
  val PossibleVs = List( 27.toByte, 28.toByte );
}
case class EthSignature( val r : BigInt, val s : BigInt ) {
  private lazy val tailByteSeq = Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* );

  lazy val exportByteSeqs = EthSignature.PossibleVs.map( _ +: tailByteSeq );
}
