package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}

import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets.UTF_8

import play.api.libs.json._

import com.mchange.sc.v2.lang.borrow

object Exchanger {
  class Simple( httpUrl : URL ) extends Exchanger {
    def exchange( methodName : String, paramsArray : JsArray )( implicit ec : ExecutionContext ) : Future[Response.Success] = Future {

      val id = scala.util.Random.nextInt()

      val paramsJsObj = JsObject( Seq( "jsonrpc" -> JsString("2.0"), "method" -> JsString(methodName), "params" ->  paramsArray, "id" -> JsNumber(id) ) )
      val paramsBytes = Json.asciiStringify( paramsJsObj ).getBytes( UTF_8 )

      val htconn = httpUrl.openConnection().asInstanceOf[HttpURLConnection]
      htconn.setDoOutput( true )
      htconn.setInstanceFollowRedirects( false )
      htconn.setUseCaches( false )
      htconn.setRequestMethod( "POST" );
      htconn.setRequestProperty( "Content-Type", "application/json");
      htconn.setRequestProperty( "charset", "utf-8" );
      htconn.setRequestProperty( "Content-Length", paramsBytes.length.toString );

      borrow( htconn.getOutputStream() )( _.write(paramsBytes) )
      borrow( htconn.getInputStream() )( Json.parse ).as[Response] match { 
        case Right( success ) => success.ensuring( _.id == id )
        case Left( error )    => throw new Failure( error )
      }
    }
    def close() : Unit = ()
  }
}
trait Exchanger extends AutoCloseable {
  def exchange( methodName : String, paramsArray : JsArray )( implicit ec : ExecutionContext ) : Future[Response.Success]
  def close() : Unit
}
