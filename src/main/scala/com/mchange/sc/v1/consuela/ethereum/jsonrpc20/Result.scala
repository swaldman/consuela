package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._

import play.api.libs.json._

object Result {
  abstract class JsStringBigIntFormat[T <: Result]( creator : BigInt => T )( extractor : T => BigInt )  extends Format[T]{
    def reads( jsv : JsValue ) : JsResult[T] = jsv.validate[JsString].map( str => creator( decodeQuantity( str ) ) )
    def writes( result : T )   : JsValue     = encodeQuantity( extractor( result ) )
  }
  final object eth {
    implicit val EthCompileSolidityFormat = new Format[eth.compileSolidity] {
      def reads( jsv : JsValue )                 : JsResult[eth.compileSolidity] = jsv.validate[immutable.Map[String,Compilation.Contract]].map( eth.compileSolidity.apply )
      def writes( result : eth.compileSolidity ) : JsValue                       = Json.toJson( result.compilations )
    }

    implicit object EthEstimateGasFormat extends JsStringBigIntFormat[eth.estimateGas]( eth.estimateGas.apply )( _.gas )

    implicit object EthGasPriceFormat extends JsStringBigIntFormat[eth.gasPrice]( eth.gasPrice.apply )( _.price )

    implicit val EthGetCompilersFormat = new Format[eth.getCompilers] {
      def reads( jsv : JsValue )              : JsResult[eth.getCompilers] = jsv.validate[immutable.Set[String]].map( eth.getCompilers.apply )
      def writes( result : eth.getCompilers ) : JsValue                    = Json.toJson( result.compilers )
    }
    implicit val EthGetTransactionCount = new Format[eth.getTransactionCount] {
      def reads( jsv : JsValue )                     : JsResult[eth.getTransactionCount] = jsv.validate[JsString].map( str => eth.getTransactionCount( decodeQuantity(str) ) )
      def writes( result : eth.getTransactionCount ) : JsValue                           = encodeQuantity( result.count )
    }
    final case class compileSolidity( compilations : immutable.Map[String,Compilation.Contract] ) extends Result
    final case class estimateGas( gas : BigInt ) extends Result;
    final case class gasPrice( price : BigInt ) extends Result;
    final case class getCompilers( compilers : immutable.Set[String] ) extends Result;
    final case class getTransactionCount( count : BigInt ) extends Result
  }
}
sealed trait Result


