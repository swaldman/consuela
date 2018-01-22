package com.mchange.sc.v1.consuela.ethereum.ethabi

import com.mchange.sc.v1.consuela.ethereum.{jsonrpc, EthHash}
import jsonrpc.Abi

object Decoded {
  case class Value( parameter : Abi.Parameter, value : Any, stringRep : String ) extends Decoded
  case class Hash( parameter : Abi.Parameter, hash : EthHash ) extends Decoded // event arguments of dynamic types can only be decoded has hashes :(
}
sealed trait Decoded {
  def parameter : Abi.Parameter
}
