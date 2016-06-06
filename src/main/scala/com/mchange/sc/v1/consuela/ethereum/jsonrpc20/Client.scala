package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.concurrent.{ExecutionContext,Future}

import java.net.URL

import play.api.libs.json._

object Client {
  trait eth {
    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Result.eth.compileSolidity]
    def getCompilers()( implicit ec : ExecutionContext ) : Future[Result.eth.getCompilers]
  }
  class withExchanger( exchanger : Exchanger ) extends Client {
    private def doExchange[T <: Result]( methodName : String, params : Seq[JsValue] )( resultBuilder : Response.Success => T )( implicit ec : ExecutionContext ) : Future[T] = {
      exchanger.exchange( methodName, JsArray( params ) ).map( resultBuilder )
    }

    val eth = new Client.eth {
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Result.eth.compileSolidity] = {
        doExchange( "eth_compileSolidity", Seq(JsString( solidityText )) )( _.result.as[Result.eth.compileSolidity] )
      }
      def getCompilers()( implicit ec : ExecutionContext ) : Future[Result.eth.getCompilers] = {
        doExchange( "eth_getCompilers", Seq() )( _.result.as[Result.eth.getCompilers] )
      }
    }

    def close() = exchanger.close()
  }
  final object Simple {
    def apply( httpUrl : URL ) = new Client.Simple( httpUrl )
  }
  final class Simple( httpUrl : URL ) extends Client.withExchanger( new Exchanger.Simple( httpUrl ) )
}
trait Client extends AutoCloseable {
  def eth : Client.eth;

  def close()
}
