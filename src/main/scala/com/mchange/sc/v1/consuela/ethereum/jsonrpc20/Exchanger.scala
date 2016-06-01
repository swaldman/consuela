package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}

import play.api.libs.json._

trait Exchanger extends AutoCloseable {
  def exchange( paramsValue : JsValue )( implicit ec : ExecutionContext ) : Future[Response]
  def close() : Unit
}
