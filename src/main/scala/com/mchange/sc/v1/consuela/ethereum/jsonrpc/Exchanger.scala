package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import scala.collection._
import scala.concurrent.{blocking,ExecutionContext,Future}

import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets.UTF_8

import play.api.libs.json._

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v2.yinyang._

import com.mchange.sc.v1.log.MLevel._

// a bit annoying to have to do this, just to refer to jsonrpc, the current package
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

object Exchanger {
  private implicit val logger = mlogger( this )

  private def traceParse( is : InputStream ) : JsValue = {
    TRACE.logEval( "Raw parsed JSON: " )( Json.parse( is ) )
  }

  trait Factory {
    def apply( url : URL ) : Exchanger
  }

  final object Simple extends Factory {
    def apply( url : URL ) : Exchanger = new Simple( url )
  }
  class Simple( httpUrl : URL ) extends Exchanger {
    TRACE.log( s"${this} created, using URL '$httpUrl'" )

    def exchange( methodName : String, paramsArray : JsArray )( implicit ec : ExecutionContext ) : Future[Response.Success] = Future {

      val id = scala.util.Random.nextInt()

      val paramsJsObj = JsObject( Seq( "jsonrpc" -> JsString("2.0"), "method" -> JsString(methodName), "params" ->  paramsArray, "id" -> JsNumber(id) ) )
      val paramsBytes = Json.asciiStringify( paramsJsObj ).getBytes( UTF_8 )

      val htconn = httpUrl.openConnection().asInstanceOf[HttpURLConnection]
      htconn.setDoOutput( true )
      htconn.setInstanceFollowRedirects( false )
      htconn.setUseCaches( false )
      htconn.setRequestMethod( "POST" );
      htconn.setRequestProperty( "Content-Type", "application/json")
      htconn.setRequestProperty( "charset", "utf-8" )
      htconn.setRequestProperty( "Content-Length", paramsBytes.length.toString )

      blocking {
        borrow( htconn.getOutputStream() )( _.write(paramsBytes) )
        borrow( htconn.getInputStream() )( traceParse ).as[Response] match {
          case Yang( success ) => success.ensuring( _.id == id )
          case Yin( error )    => throw new jsonrpc.Exception( error )
        }
      }
    }
    def close() : Unit = ()
  }
}
trait Exchanger extends AutoCloseable {
  def exchange( methodName : String, paramsArray : JsArray )( implicit ec : ExecutionContext ) : Future[Response.Success]
  def close() : Unit
}
