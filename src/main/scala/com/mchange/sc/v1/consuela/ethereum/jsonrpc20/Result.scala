package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._

import play.api.libs.json._

object Result {
  final object Eth {
    implicit val EthCompileSolidityFormat = Json.format[compileSolidity]
    case class compileSolidity( compilations : immutable.Map[String,Compilation.Contract] ) extends Result
  }
}
sealed trait Result


