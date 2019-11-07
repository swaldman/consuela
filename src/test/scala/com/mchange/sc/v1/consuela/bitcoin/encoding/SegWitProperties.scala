package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin._

import com.mchange.sc.v1.consuela.ethereum.specification.Arbitraries._

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}

import scala.collection._

object SegWitProperties extends Properties("SegWit") {

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
    val encoded = SegWit.encode( "bc", startTuple._1, startTuple._2 )
    val decoded = SegWit.decode( Some("bc"), encoded )
    decoded == startTuple
  }

  property("tbOnlyZeroTwentyRoundtrips") = forAll { ( bse20 : ByteSeqExact20 ) =>
    val startTuple = ( Byte0, bse20.widen )
    val encoded = SegWit.encode( "tb", startTuple._1, startTuple._2 )
    val decoded = SegWit.decode( Some("tb"), encoded )
    decoded == startTuple
  }

  property("variableHrpZeroTwentyRoundtripsWithExpectation") = forAllNoShrink( HrpGen, genRandomImmutableByteSeqN(20) ) { ( hrp : String, seq20 : immutable.Seq[Byte] ) =>
    val startTuple = ( Byte0, seq20 )
    val encoded = SegWit.encode( hrp, startTuple._1, startTuple._2 )
    val decoded = SegWit.decode( Some(hrp), encoded )
    decoded == startTuple
  }

  property("variableHrpZeroTwentyRoundtripsNoExpectation") = forAllNoShrink( HrpGen, genRandomImmutableByteSeqN(20) ) { ( hrp : String, seq20 : immutable.Seq[Byte] ) =>
    val startTuple = ( Byte0, seq20 )
    val encoded = SegWit.encode( hrp, startTuple._1, startTuple._2 )
    val decoded = SegWit.decode( None, encoded )
    decoded == startTuple
  }

}
