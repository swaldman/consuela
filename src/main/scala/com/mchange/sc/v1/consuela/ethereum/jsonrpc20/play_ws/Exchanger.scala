package com.mchange.sc.v1.consuela.ethereum.jsonrpc20.play_ws

import scala.concurrent.{ExecutionContext,Future}

import play.api.libs.json._
import play.api.libs.ws._

import akka.stream.ActorMaterializer

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.{Exchanger => GenericExchanger,Response}

object Exchanger {
  val DefaultAhcClientConfig = {
    val wsClientConfig = WSClientConfig( followRedirects = false, useProxyProperties = false )
    ahc.AhcWSClientConfig( maxConnectionsPerHost = 5, maxConnectionsTotal = 5 )
  }
  def apply( url : String, clientConfig : ahc.AhcWSClientConfig = DefaultAhcClientConfig )( implicit manager : Manager ) : Exchanger = {
    manager.synchronized {
      val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
      val out = new Exchanger( url, ahc.AhcWSClient( configBuilder.build() )( manager.materializer ), Some(manager) )
      manager.register( out )
      out
    }
  }
  // because only one overload can have default args, and we can only overload on 1st argument lost
  def _apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : Exchanger = {
    val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
    new Exchanger( url, ahc.AhcWSClient( configBuilder.build() )( materializer ), None )
  }
  def apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : Exchanger = _apply( url, clientConfig )( materializer )
  def apply( url : String )( implicit materializer : ActorMaterializer ) : Exchanger                                       = _apply( url, DefaultAhcClientConfig )( materializer )
}
class Exchanger private ( url : String, client : play.api.libs.ws.WSClient, mbManager : Option[Manager] ) extends GenericExchanger {
  def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response] = {
    client.url( url ).withFollowRedirects( false ).post( JsObject( Seq( "params" ->  paramsValue ) ) ).map( wsresponse => wsresponse.json.as[Response] )
  }
  def close() : Unit = {
    mbManager.foreach{ manager =>
      WARNING.attemptWithLabel(s"Attempt to deregister ${this} failed")( manager.deregister( this ) )
    }
    client.close()
  }
}

