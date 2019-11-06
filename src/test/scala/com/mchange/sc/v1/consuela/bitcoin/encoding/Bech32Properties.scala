package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin._

import com.mchange.sc.v1.consuela.ethereum.specification.Arbitraries._

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}

import scala.collection._

object Bech32Properties extends Properties("Bech32") {

  private final val Byte0 = 0.toByte

  val HrpGen = {
    for {
      // n <- Gen.choose(1, 83) // yields addresses too long, longer than max 90 allowable chars
      n <- Gen.choose(1, 50) 
      arr <- Gen.containerOfN[Array,Char]( n, Gen.choose( 33, 126 ) )
      flip <- Gen.choose(0,1)
    }
    yield {
      val raw = new String( arr )
      flip match {
        case 0 => raw.toLowerCase
        case 1 => raw.toUpperCase
      }
    }
  }

  property("bcOnlyZeroTwentyRoundtrips") = forAll { ( bse20 : ByteSeqExact20 ) =>
    val startTuple = ( Byte0, bse20.widen )
    val encoded = Bech32.encodeSegWit( "bc", startTuple._1, startTuple._2 )
    val decoded = Bech32.decodeSegWit( "bc", encoded )
    decoded == startTuple
  }

  property("tbOnlyZeroTwentyRoundtrips") = forAll { ( bse20 : ByteSeqExact20 ) =>
    val startTuple = ( Byte0, bse20.widen )
    val encoded = Bech32.encodeSegWit( "tb", startTuple._1, startTuple._2 )
    val decoded = Bech32.decodeSegWit( "tb", encoded )
    decoded == startTuple
  }

  property("variableHrpZeroTwentyRoundtrips") = forAllNoShrink( HrpGen, genRandomImmutableByteSeqN(20) ) { ( hrp : String, seq20 : immutable.Seq[Byte] ) =>
    val startTuple = ( Byte0, seq20 )
    val encoded = Bech32.encodeSegWit( hrp, startTuple._1, startTuple._2 )
    val decoded = Bech32.decodeSegWit( hrp, encoded )
    decoded == startTuple
  }

}
