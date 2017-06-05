package com.mchange.sc.v1.consuela.ethereum

import scala.language.implicitConversions

/**
  * Just a container for sometimes-convenient implicit conversions to EthAddress
  */ 
object EthAddresses extends EthAddresses

/**
  * An extendable container for sometimes-convenient implicit conversions to EthAddress
  */ 
trait EthAddresses {
  implicit def hexStringToAddress( hex : String )    : EthAddress = EthAddress( hex )
  implicit def arrayToAddress( bytes : Array[Byte] ) : EthAddress = EthAddress( bytes )
  implicit def seqToAddress( bytes : Seq[Byte] )     : EthAddress = EthAddress( bytes )
}
