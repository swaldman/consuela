package com.mchange.sc.v1.consuela.ethereum

import scala.collection._

import org.specs2._;

class EthAddressChecksumSpec extends Specification {

  val GoodEip55MixedCaseAddresses = immutable.Seq(
    "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed",
    "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
    "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB",
    "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb",

    // same as above, but with no prefix
    "5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed",
    "fB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
    "dbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB",
    "D1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
  )

  val BadEip55MixedCaseAddresses = immutable.Seq( // each one modified, by digit or case, in one place
    "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAe7",
    "0xfB6916095ca1df60bB79Ce92cE3ea74c37c5d359",
    "0xdbF03B407c01E2cD3CBea99509d93f8DDDC8C6FB",
    "0xD1220A0cf47C7B9Be7A2E6BA89F429762e7b9aDb",

    // same as above, but with no prefix
    "5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAe7",
    "fB6916095ca1df60bB79Ce92cE3ea74c37c5d359",
    "dbF03B407c01E2cD3CBea99509d93f8DDDC8C6FB",
    "D1220A0cf47C7B9Be7A2E6BA89F429762e7b9aDb"
  )

  val GoodRSKIP60MixedCaseAddresses = {
    val prefixed = {
      immutable.Seq(
        Some( EthChainId( 30 ) ) -> "0x5aaEB6053f3e94c9b9a09f33669435E7ef1bEAeD",
        Some( EthChainId( 30 ) ) -> "0xFb6916095cA1Df60bb79ce92cE3EA74c37c5d359",
        Some( EthChainId( 30 ) ) -> "0xDBF03B407c01E7CD3cBea99509D93F8Dddc8C6FB",
        Some( EthChainId( 30 ) ) -> "0xD1220A0Cf47c7B9BE7a2e6ba89F429762E7B9adB",
        Some( EthChainId( 31 ) ) -> "0x5aAeb6053F3e94c9b9A09F33669435E7EF1BEaEd",
        Some( EthChainId( 31 ) ) -> "0xFb6916095CA1dF60bb79CE92ce3Ea74C37c5D359",
        Some( EthChainId( 31 ) ) -> "0xdbF03B407C01E7cd3cbEa99509D93f8dDDc8C6fB",
        Some( EthChainId( 31 ) ) -> "0xd1220a0CF47c7B9Be7A2E6Ba89f429762E7b9adB",
        None -> "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed",
        None -> "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
        None -> "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB",
        None -> "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
      )
    }
    val unprefixed = prefixed.map { case ( mbChainId, prefixedHex ) => ( mbChainId, prefixedHex.drop(2) ) }
    prefixed ++ unprefixed
  }

  val BadRSKIP60GoodEIP55MixedCaseAddresses = {
    val prefixed = {
      immutable.Seq(
        Some( EthChainId( 30 ) ) -> "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed",
        Some( EthChainId( 30 ) ) -> "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
        Some( EthChainId( 30 ) ) -> "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB",
        Some( EthChainId( 30 ) ) -> "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb",
        Some( EthChainId( 31 ) ) -> "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed",
        Some( EthChainId( 31 ) ) -> "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
        Some( EthChainId( 31 ) ) -> "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB",
        Some( EthChainId( 31 ) ) -> "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
      )
    }
    val unprefixed = prefixed.map { case ( mbChainId, prefixedHex ) => ( mbChainId, prefixedHex.drop(2) ) }
    prefixed ++ unprefixed
  }

  def parses( expected : Boolean )( hex : String ) : Boolean = {
    try {
      val parsed = EthAddress( hex )
      if (!expected) println( s"Unexpected success: ${hex}" )
      true
    }
    catch {
      case bce : EthAddress.BadMixedCaseChecksumException => {
        if (expected) println( s"Unexpected failure: ${hex}" )
        false
      }
      case t : Throwable => {
        println( s"bad hex: ${hex}" )
        throw t
      }
    }
  }

  def parsesStrictly( expected : Boolean )( mbChainId : Option[EthChainId], hex : String ) : Boolean = {
    try {
      val parsed = EthAddress.parseStrictly( hex, mbChainId )
      if (!expected) println( s"Unexpected success: ${hex} ${mbChainId}" )
      true
    }
    catch {
      case bce : EthAddress.BadMixedCaseChecksumException => {
        if (expected) println( s"Unexpected failure: ${hex} ${mbChainId}" )
        false
      }
      case t : Throwable => {
        println( s"${mbChainId} -> ${hex}" )
        throw t
      }
    }
  }
  
  def parsesPermissively( expected : Boolean )( mbChainId : Option[EthChainId], hex : String ) : Boolean = {
    try {
      val parsed = EthAddress.parsePermissively( hex, mbChainId )
      if (!expected) println( s"Unexpected success: ${hex} ${mbChainId}" )
      true
    }
    catch {
      case bce : EthAddress.BadMixedCaseChecksumException => {
        if (expected) println( s"Unexpected failure: ${hex} ${mbChainId}" )
        false
      }
      case t : Throwable => {
        println( s"${mbChainId} -> ${hex}" )
        throw t
      }
    }
  }

  def upperCaseHex( maybePrefixed : String ) : String = {
    val isPrefixed = maybePrefixed.startsWith( "0x" )
    if (isPrefixed) {
      "0x" + maybePrefixed.drop(2).toUpperCase
    }
    else {
      maybePrefixed.toUpperCase
    }
  }
  def lowerCaseHex( maybePrefixed : String ) : String = maybePrefixed.toLowerCase

  def upperCaseHex( tup : Tuple2[Option[EthChainId],String] ) : Tuple2[Option[EthChainId],String] = ( tup._1, upperCaseHex( tup._2 ) )

  def lowerCaseHex( tup : Tuple2[Option[EthChainId],String] ) : Tuple2[Option[EthChainId],String] = ( tup._1, lowerCaseHex( tup._2 ) )

  def is =
s2"""
   EthAddresses should
     good EIP-55 addresses with no chain ID specified should parse ${ e1 }
     bad EIP-55 addresses with no chain ID specified should fail to parse ${ e2 }
     good RSKIP-60 addresses with chain IDs specified should parse strictly ${ e3 }
     bad RSKIP-60 addresses with chain IDs specified that ARE good EIP-55 addresses should fail to parse strictly ${ e4 }
     bad RSKIP-60 addresses with chain IDs specified that ARE good EIP-55 addresses should succeed to parse permissively ${ e5 }
     all-lower-case versions of "bad" EIP-55 addresses should succeed to parse ${ e6 }
     all-upper-case versions of "bad" EIP-55 addresses should succeed to parse ${ e7 }
     all-lower-case versions of "bad" RSKIP-60 addresses with chain IDs specified should succeed to parse ${ e8 }
     all-upper-case versions of "bad" RSKIP-60 addresses with chain IDs specified should succeed to parse ${ e9 }
""";

  def e1 = GoodEip55MixedCaseAddresses.forall( parses(expected = true)  )
  def e2 = !BadEip55MixedCaseAddresses.exists( parses(expected = false) )
  def e3 = GoodRSKIP60MixedCaseAddresses.forall( (parsesStrictly( expected = true ) _).tupled )
  def e4 = !BadRSKIP60GoodEIP55MixedCaseAddresses.exists( (parsesStrictly( expected = false ) _).tupled )
  def e5 = BadRSKIP60GoodEIP55MixedCaseAddresses.forall( (parsesPermissively( expected = true ) _).tupled )
  def e6 = BadEip55MixedCaseAddresses.map( lowerCaseHex ).forall( parses(expected = true) )
  def e7 = BadEip55MixedCaseAddresses.map( upperCaseHex ).forall( parses(expected = true) )
  def e8 = BadRSKIP60GoodEIP55MixedCaseAddresses.map( lowerCaseHex ).forall( (parsesStrictly( expected = true ) _).tupled )
  def e9 = BadRSKIP60GoodEIP55MixedCaseAddresses.map( upperCaseHex ).forall( (parsesStrictly( expected = true ) _).tupled )
}
