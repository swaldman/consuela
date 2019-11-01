package com.mchange.sc.v1.consuela.ethereum.ethabi

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact20,ByteSeqExact32}

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import org.specs2._

object SolidityEventSpec {
  val exLogEntry = {
    EthLogEntry(
      EthAddress(
        ByteSeqExact20(
          "0x353e8352f0782b827d72757dab9cc9464c7e9a3b".decodeHexAsSeq
        )
      ),
      Vector(
        ByteSeqExact32(
          "0xadfa1f0ce4eb1d83af9464a1ab1144799ce4ec3f71e9a0478e437b4b63bafd55".decodeHexAsSeq
        )
      ),
      ImmutableArraySeq.Byte(
        "0x000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000034869210000000000000000000000000000000000000000000000000000000000".decodeHex
      )
    )
  }
  val exAbiParam = {
    Abi.Event.Parameter("payload","string",false,"string")
  }
  val exAbiEvent = {
    Abi.Event("Pinged",List( exAbiParam ),false)
  }
  val UTF8 = scala.io.Codec.UTF8.charSet
}
class SolidityEventSpec extends Specification {
  import SolidityEventSpec._

  def is =
s2"""
   SolidityEvent should
     properly decode an example log entry + abi event   ${ e1 }
"""

  def e1 = {
    val decoded = SolidityEvent.interpretLogEntryAsEvent( exLogEntry, exAbiEvent ).get.head
    val hi = "Hi!"
    val hibytes = ImmutableArraySeq.Byte(hi.getBytes( UTF8 ))
    val expected = Decoded.Value( exAbiParam, hibytes, s""""${hi}"""" ) 
    // println( s"decoded: ${decoded}" )
    // println( s"expected: ${expected}" )
    decoded == expected
  }
}
