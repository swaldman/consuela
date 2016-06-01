package com.mchange.sc.v1.consuela.ethereum.jsonrpc20.play_ws

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}

import play.api.libs.json._
import play.api.libs.ws._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.{Exchanger,Response}

object PlayWSExchanger {
  private implicit lazy val logger = mlogger( this )

  final object Manager {
    final class ClosedException( mgr : Manager ) extends Exception( s"Operation failed: ${mgr} has already been closed()." )
  }
  class Manager extends AutoCloseable {
    implicit val akkaSystem = ActorSystem("PlayWSJsonRpcClientAkka");
    val materializer = ActorMaterializer()

    def close() : Unit = {
      closeChildren()
      akkaSystem.terminate()
    }

    // MT: protected by this' lock
    private var children = mutable.HashSet.empty[PlayWSExchanger]

    def register( wsClient : PlayWSExchanger )   : Unit = this.synchronized( if ( children != null ) children += wsClient else throw new Manager.ClosedException(this) )
    def deregister( wsClient : PlayWSExchanger ) : Unit = this.synchronized( if ( children != null ) children -= wsClient else throw new Manager.ClosedException(this) )
    def closeChildren() : Unit = this.synchronized{
      children.foreach( child => WARNING.attemptWithLabel( s"Attempt by ${this} to autoclose unclosed ${classOf[PlayWSExchanger].getName} failed" )( child.close() ) )
      children = null
    }
  }

  val DefaultAhcClientConfig = {
    val wsClientConfig = WSClientConfig( followRedirects = false, useProxyProperties = false )
    ahc.AhcWSClientConfig( maxConnectionsPerHost = 5, maxConnectionsTotal = 5 )
  }
  def apply( url : String, clientConfig : ahc.AhcWSClientConfig = DefaultAhcClientConfig )( implicit manager : Manager ) : PlayWSExchanger = {
    manager.synchronized {
      val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
      val out = new PlayWSExchanger( url, ahc.AhcWSClient( configBuilder.build() )( manager.materializer ), Some(manager) )
      manager.register( out )
      out
    }
  }
  // because only one overload can have default args, and we can only overload on 1st argument lost
  def _apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : PlayWSExchanger = {
    val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
    new PlayWSExchanger( url, ahc.AhcWSClient( configBuilder.build() )( materializer ), None )
  }
  def apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : PlayWSExchanger = _apply( url, clientConfig )( materializer )
  def apply( url : String )( implicit materializer : ActorMaterializer ) : PlayWSExchanger                                       = _apply( url, DefaultAhcClientConfig )( materializer )
}
class PlayWSExchanger private ( url : String, client : play.api.libs.ws.WSClient, mbManager : Option[PlayWSExchanger.Manager] ) extends Exchanger {
  import PlayWSExchanger.logger
  
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

