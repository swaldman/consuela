package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}

import play.api.libs.json._

// TODO: DefaultAhcClientConfig
object Exchanger {

  /**
    * This seems way overcomplicated.
    * 
    * It also brings in pretty much the whole Play/Akka ecosystem via transitive dependencies.
    * 
    * Maybe segregregate and choose a simpler, cleaner HTTP client by default.
    */ 
  final object Play {
    import play.api.libs.ws._
    import akka.actor.ActorSystem
    import akka.stream.ActorMaterializer
    import com.mchange.sc.v1.log.MLevel._
    
    private implicit lazy val logger = mlogger( this )

    object WSClient {
      object Manager {
        final class ClosedException( mgr : Manager ) extends Exception( s"Operation failed: ${mgr} has already been closed()." )
      }
      class Manager extends AutoCloseable {
        implicit val akkaSystem = ActorSystem("PlayWSClientAkka");
        val materializer = ActorMaterializer()

        def close() : Unit = {
          closeChildren()
          akkaSystem.terminate()
        }

        // MT: protected by this' lock
        private var children = mutable.HashSet.empty[WSClient]

        def register( wsClient : WSClient )   : Unit = this.synchronized( if ( children != null ) children += wsClient else throw new Manager.ClosedException(this) )
        def deregister( wsClient : WSClient ) : Unit = this.synchronized( if ( children != null ) children -= wsClient else throw new Manager.ClosedException(this) )
        def closeChildren() : Unit = this.synchronized{
          children.foreach( child => WARNING.attemptWithLabel( s"Attempt by ${this} to autoclose unclosed Exchanger.Play.WSClient failed" )( child.close() ) )
          children = null
        }
      }

      val DefaultAhcClientConfig = {
        val wsClientConfig = WSClientConfig( followRedirects = false, useProxyProperties = false )
        ahc.AhcWSClientConfig( maxConnectionsPerHost = 5, maxConnectionsTotal = 5 )
      }
      def apply( url : String, clientConfig : ahc.AhcWSClientConfig = DefaultAhcClientConfig )( implicit manager : Manager ) : WSClient = {
        manager.synchronized {
          val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
          val out = new WSClient( url, ahc.AhcWSClient( configBuilder.build() )( manager.materializer ), Some(manager) )
          manager.register( out )
          out
        }
      }
      // because only one overload can have default args, and we can only overload on 1st argument lost
      def _apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : WSClient = {
        val configBuilder = new ahc.AhcConfigBuilder( clientConfig )
        new WSClient( url, ahc.AhcWSClient( configBuilder.build() )( materializer ), None )
      }
      def apply( url : String, clientConfig : ahc.AhcWSClientConfig )( implicit materializer : ActorMaterializer ) : WSClient = _apply( url, clientConfig )( materializer )
      def apply( url : String )( implicit materializer : ActorMaterializer ) : WSClient                                       = _apply( url, DefaultAhcClientConfig )( materializer )
    }
    class WSClient private ( url : String, client : play.api.libs.ws.WSClient, mbManager : Option[WSClient.Manager] ) extends Exchanger {
      def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response] = {
        client.url( url ).withFollowRedirects( false ).post( JsObject( Seq( "params" ->  paramsValue ) ) ).map( wsresponse => wsresponse.json.as[Response] )
      }
      def close() : Unit = {
        mbManager.foreach{ manager =>
          WARNING.attemptWithLabel("Attempt to deregister an Exchanger.Play.WSClient failed")( manager.deregister( this ) )
        }
        client.close()
      }
    }
  }
}
trait Exchanger extends AutoCloseable {
  def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response]
  def close() : Unit
}
