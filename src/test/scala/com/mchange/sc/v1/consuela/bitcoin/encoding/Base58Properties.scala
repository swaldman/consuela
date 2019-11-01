package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

object Base58Properties extends Properties("Base58") {

  property("bigIntRoundTrips") = forAll { ( bi : BigInt ) =>
    bi >= 0 ==> (Base58.toBigInt( Base58.fromBigInt(bi) ) == bi)
  }

  property("bigIntNoLeadingOnes") = forAll { ( bi : BigInt ) =>
    // zero is the empty string, so we exclude it from the test
    bi > 0 ==> (Base58.fromBigInt(bi)(0) != '1')
  }

  property("byteArrayRoundTrips") = forAll { ( barr : Array[Byte] ) => 
    val b58 = Base58.fromByteArray( barr )
    val out = java.util.Arrays.equals( Base58.toByteArray( b58 ), barr )
    if (!out) println( s"0x${barr.hex} converted to '${b58}' and failed to roundtrip!" ) 
    out
  }

}
