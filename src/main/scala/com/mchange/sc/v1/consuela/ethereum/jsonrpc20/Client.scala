package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.concurrent.{ExecutionContext,Future}
import scala.util.control.NonFatal

import java.net.URL

import play.api.libs.json._

import com.mchange.sc.v2.failable._

object Client {
  trait Eth {
    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Failable[Result.Eth.compileSolidity]]
  }
  class withExchanger( exchanger : Exchanger ) {
    private def doExchange[T <: Result]( methodName : String, params : Seq[JsValue], resultBuilder : Response.Success => T )( implicit ec : ExecutionContext ) : Future[Failable[T]] = {
      exchanger.exchange( methodName, JsArray( params ) )
        .map( response => toFailable( response ).map( resultBuilder ) )
        .recover{ case NonFatal( t ) => fail(t) }
    }

    val Eth = new Client.Eth {
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Failable[Result.Eth.compileSolidity]] = {
        doExchange( "eth_compileSolidity", Seq(JsString( solidityText )), success => success.result.as[Result.Eth.compileSolidity] )
      }
    }
  }
  final object Simple {
    def apply( httpUrl : URL ) = new Client.Simple( httpUrl )
  }
  final class Simple( httpUrl : URL ) extends Client.withExchanger( new Exchanger.Simple( httpUrl ) )
}
trait Client extends AutoCloseable {
  def Eth : Client.Eth;

  def close()
}
