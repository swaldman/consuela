package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.concurrent.{ExecutionContext,Future}
import scala.util.control.NonFatal

import play.api.libs.json._

import com.mchange.sc.v2.failable._

object Client {
  trait Eth {
    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Failable[Result.Eth.compileSolidity]]
  }
  class withExchanger( exchanger : Exchanger ) {
    private def doExchange[T <: Result]( methodName : String, paramsValue : JsValue, resultBuilder : Response.Success => T )( implicit ec : ExecutionContext ) : Future[Failable[T]] = {
      exchanger.exchange( methodName, paramsValue )
        .map( response => toFailable( response ).map( resultBuilder ) )
        .recover{ case NonFatal( t ) => fail(t) }
    }

    val Eth = new Client.Eth {
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Failable[Result.Eth.compileSolidity]] = {
        doExchange( "eth_compileSolidity", JsString( solidityText ), Result.Eth.compileSolidity.apply )
      }
    }
  }
}
trait Client {
  def Eth : Client.Eth;

  def close()
}
