package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin._

import com.mchange.sc.v1.consuela.ethereum.specification.Arbitraries._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

object Bech32Properties extends Properties("Bech32") {

  private final val Byte0 = 0.toByte

  property("bcOnlyZeroTwentyRoundtrips") = forAll { ( bse20 : ByteSeqExact20 ) =>
    val startTuple = ( Byte0, bse20.widen )
    val encoded = Bech32.encodeSegWit( "bc", startTuple._1, startTuple._2 )
    val decoded = Bech32.decodeSegWit( "bc", encoded )
    decoded == startTuple
  }
}
