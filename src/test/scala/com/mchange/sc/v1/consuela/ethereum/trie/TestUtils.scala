package com.mchange.sc.v1.consuela.ethereum.trie;

import play.api.libs.json._;

object TestUtils {
  val UnexpectedJsValue : PartialFunction[JsValue,Nothing] = {
    case JsNull              => throw new AssertionError( s"Null input value not expected: ${JsNull}" );
    case und   : JsUndefined => throw new AssertionError( s"Undefined input value not expected: ${und}" );
    case array : JsArray     => throw new AssertionError( s"Array input value not expected: ${array}" );
    case bool  : JsBoolean   => throw new AssertionError( s"Boolean input value not expected: ${bool}" );
    case num   : JsNumber    => throw new AssertionError( s"Number input value not expected: ${num}" );
    case obj   : JsObject    => throw new AssertionError( s"Object input value not expected: ${obj}" );
    case str   : JsString    => throw new AssertionError( s"String input value not expected: ${str}" );
  }
  def toStringField( field : Tuple2[String,JsValue] ) : Tuple2[String,String] = ( field._1, field._2.asInstanceOf[JsString].value );
  def toStringFields( fields : Seq[Tuple2[String,JsValue]] ) : Seq[Tuple2[String,String]] = fields.map( toStringField _ );
  def printEval[T]( expr : =>T, mbPrefix : Option[String] = None) : T = {
    val out = expr;
    mbPrefix.fold( println( out ) )( pfx => println( s"${pfx}${out}" ) )
    out
  }
}
