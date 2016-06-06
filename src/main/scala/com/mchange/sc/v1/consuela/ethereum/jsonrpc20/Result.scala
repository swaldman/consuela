package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._

import play.api.libs.json._

object Result {
  final object eth {
    implicit val EthCompileSolidityFormat = new Format[eth.compileSolidity] {
      def reads( jsv : JsValue )                 : JsResult[eth.compileSolidity] = jsv.validate[immutable.Map[String,Compilation.Contract]].map( eth.compileSolidity.apply )
      def writes( result : eth.compileSolidity ) : JsValue                       = Json.toJson( result.compilations )
    }
    implicit val EthGetCompilersFormat = new Format[eth.getCompilers] {
      def reads( jsv : JsValue )              : JsResult[eth.getCompilers] = jsv.validate[immutable.Set[String]].map( eth.getCompilers.apply )
      def writes( result : eth.getCompilers ) : JsValue                    = Json.toJson( result.compilers )
    }
    case class compileSolidity( compilations : immutable.Map[String,Compilation.Contract] ) extends Result
    case class getCompilers( compilers : immutable.Set[String] ) extends Result;
  }
}
sealed trait Result


