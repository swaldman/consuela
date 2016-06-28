package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._

import play.api.libs.json._

object Result {
  final object eth {
    implicit val EthCompileSolidityFormat = new Format[eth.compileSolidity] {
      def reads( jsv : JsValue )                 : JsResult[eth.compileSolidity] = jsv.validate[immutable.Map[String,Compilation.Contract]].map( eth.compileSolidity.apply )
      def writes( result : eth.compileSolidity ) : JsValue                       = Json.toJson( result.compilations )
    }
    implicit val EthGasPrice = new Format[eth.gasPrice] {
      def reads( jsv : JsValue )          : JsResult[eth.gasPrice] = jsv.validate[JsString].map( str => eth.gasPrice( decodeQuantity( str ) ) )
      def writes( result : eth.gasPrice ) : JsValue                = encodeQuantity( result.price )
    }
    implicit val EthGetCompilersFormat = new Format[eth.getCompilers] {
      def reads( jsv : JsValue )              : JsResult[eth.getCompilers] = jsv.validate[immutable.Set[String]].map( eth.getCompilers.apply )
      def writes( result : eth.getCompilers ) : JsValue                    = Json.toJson( result.compilers )
    }
    implicit val EthGetTransactionCount = new Format[eth.getTransactionCount] {
      def reads( jsv : JsValue )                     : JsResult[eth.getTransactionCount] = jsv.validate[JsString].map( str => eth.getTransactionCount( decodeQuantity(str) ) )
      def writes( result : eth.getTransactionCount ) : JsValue                           = encodeQuantity( result.count )
    }
    final case class compileSolidity( compilations : immutable.Map[String,Compilation.Contract] ) extends Result
    final case class gasPrice( price : Long ) extends Result;
    final case class getCompilers( compilers : immutable.Set[String] ) extends Result;
    final case class getTransactionCount( count : Long ) extends Result
  }
}
sealed trait Result


