package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._

import play.api.libs.json._

object Result {
  trait Factory[T <: Result] {
    def apply( result : JsValue ) : T;
    def apply( response : Response.Success ) : T = apply( response.result ) 
  }
  final object Eth {
    object compileSolidity extends Result.Factory[Result.Eth.compileSolidity]{
      def apply( result : JsValue ) : Result.Eth.compileSolidity = Result.Eth.compileSolidity( result.as[immutable.Map[String,Compilation.Contract]] )
    }
    case class compileSolidity( compilations : immutable.Map[String,Compilation.Contract] ) extends Result
  }
}
sealed trait Result


