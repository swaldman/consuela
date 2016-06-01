package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.concurrent.{ExecutionContext,Future}

import play.api.libs.json._

object Client {
  trait Eth {
    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Result.Eth.compileSolidity]
  }
  class withExchanger( exchanger : Exchanger ) {
    val Eth = new Client.Eth {
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[Result.Eth.compileSolidity] = {
        exchanger.exchange( JsString( solidityText ) ).map( Result.Eth.compileSolidity.apply )
      }
    }
  }
}
trait Client {
  def Eth : Client.Eth;

  def close()
}
