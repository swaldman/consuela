package com.mchange.sc.v1.consuela.ethereum.encoding;

import scala.io.Source;

import play.api.libs.json._;

import java.nio.charset.Charset;

import com.mchange.lang.ByteUtils;

import org.specs2._;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

object RLPSpec {
  object EthereumRLPTest {
    private[this] implicit val charset = Charset.forName("UTF-8");

    val RsrcPath : String          = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-rlptest.json"
    lazy val JsonString : String   = Source.fromURL(this.getClass.getResource(RsrcPath)).mkString
    lazy val asJsObject : JsObject = Json.parse( JsonString ) match {
      case jso : JsObject => jso;
      case _ => throw new AssertionError("Ethereum's rlptest.json is expected to parse to a JsObject!");
    };
    lazy val items : Seq[Item] = EthereumRLPTest.asJsObject.fields.map( Item( _ ) )

    object Item {
      private[this] def parseIn( in : JsValue )( implicit charset : Charset ) : RLP.Element = {
        in match {
          case JsNull              => throw new AssertionError( s"Null input values not expected: ${JsNull}" );
          case und   : JsUndefined => throw new AssertionError( s"Undefined input values not expected: ${und}" );
          case array : JsArray     => RLP.Element.Seq( List( array.value.map( parseIn ) : _*) );
          case bool  : JsBoolean   => throw new AssertionError( s"Boolean input values not expected: ${bool}" );
          case num   : JsNumber    => RLP.Element.UnsignedInt( num.value.toIntExact );
          case obj   : JsObject    => throw new AssertionError( s"Object input values not expected: ${obj}" );
          case str   : JsString    => {
            if ( str.value.length > 0 && str.value.charAt(0) == '#' )
              RLP.Element.UnsignedBigInt( BigInt( str.value.substring(1) ) ) 
            else
              RLP.Element.ByteSeq( str.value.getBytes( charset ) );
          }
        }
      }
      private[this] def parseOut( out : String ) : Seq[Byte] = ByteUtils.fromHexAscii( out ).toSeq;
      def apply( field : ( String, JsValue ) )( implicit charset : Charset ) : Item = apply( field._1, field._2 )( charset )
      def apply( name : String, jsv : JsValue )( implicit charset : Charset ) : Item = {
        val in  = parseIn( jsv \ "in" );
        val out = parseOut( (jsv \ "out").asInstanceOf[JsString].value );
        Item( name, in, out )
      }
    }
    case class Item( name : String, in : RLP.Element, out : Seq[Byte] ) {
      lazy val ( element, rest ) = RLP.Element.decode(out);

      def test : Boolean = RLP.Element.encode( in ) == out

      def reverseTest : Boolean = (RLP.Element.sameBytes(in, element) && rest.length == 0);

      def verboseTest : Boolean = {
        val result = test;
        println( s"test '${name}': ${result}" );
        if (! result )
          println(s"      expected: ${ByteUtils.toHexAscii(out.toArray)}    encoded: ${ByteUtils.toHexAscii( (RLP.Element.encode( in )).toArray )}");
        result
      }
      def verboseReverseTest : Boolean = {
        try {
          //println( s"[Beginning reverse-test '$name']" );
          val result = reverseTest;
          println( s"reverse-test '${name}': ${result}" );
          if (! result )
            println(s"      expected: ${in}    decoded: ${element}    rest: ${rest}    sameBytes: ${RLP.Element.sameBytes(in, element)}");
          result
        } catch {
          case t : Throwable => {
            println( s"reverse-test '${name}': false" );
            println( s"      expected: ${in}    decoded: ${element}    rest: ${rest}    sameBytes: ${RLP.Element.sameBytes(in, element)}" );
            println( s"      ${t}" );
            t.printStackTrace();
            false
          }
        }
      }
    }
  }
}
class RLPSpec extends Specification { 

  private[this] implicit val logger = MLogger( this );

  def is =
s2"""
   RLP should
     pass the ethereum RLP test suite             ${ e1 }
     pass the ethereum RLP test suite in reverse  ${ e2 }
""";

  def e1 = RLPSpec.EthereumRLPTest.items.forall( /* _.verboseTest */ _.test )
  def e2 = RLPSpec.EthereumRLPTest.items.forall( /* _.verboseReverseTest */ _.reverseTest )

  def printEval[T]( expr : =>T ) : T = { val out = expr; println( "===> " + out ); out }
}
