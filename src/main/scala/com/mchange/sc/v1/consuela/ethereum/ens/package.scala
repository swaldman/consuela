package com.mchange.sc.v1.consuela.ethereum

import java.net.IDN
import java.nio.charset.StandardCharsets.US_ASCII

/**
  *  See https://github.com/ethereum/EIPs/issues/137
  */ 
package object ens {

  private val NullHash = EthHash.withBytes( Array.fill[Byte](32)(0.toByte) )

  private def tokenizeReverse( name : String ) : List[String] = {
    val arr = if ( name.length == 0 ) Array.empty[String] else name.split("""\.""")
    val len = arr.length

    def build( nextIndex : Int, accum : List[String] ) : List[String] = {
      nextIndex match {
        case `len` => accum
        case i     => build( i + 1, arr(i) :: accum )
      }
    }

    build(0, Nil)
  }
  def namehash( name : String ) : EthHash = {
    val components = tokenizeReverse( name )
    components.foldLeft( NullHash ) { ( last, next ) =>
      EthHash.hash( last.bytes ++ EthHash.hash( IDN.toASCII( next, IDN.USE_STD3_ASCII_RULES ).getBytes( US_ASCII ) ).bytes )
    }
  }
}

