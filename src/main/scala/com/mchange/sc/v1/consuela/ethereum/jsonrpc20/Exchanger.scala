package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}

import java.net.{HttpURLConnection, URL}
import java.nio.charset.StandardCharsets.UTF_8

import play.api.libs.json._

import com.mchange.sc.v2.lang.borrow

object Exchanger {
  class Simple( httpUrl : URL ) extends Exchanger {
    def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response] = Future {

      val paramsJsObj = JsObject( Seq( "params" ->  paramsValue ) )
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
      borrow( htconn.getInputStream() )( Json.parse ).as[Response]
    }
    def close() : Unit = ()
  }
}
trait Exchanger extends AutoCloseable {
  def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response]
  def close() : Unit
}
