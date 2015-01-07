package com.mchange.sc.v1.consuela.ethereum;

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
      private[this] def parseIn( in : JsValue )( implicit charset : Charset ) : RLP.Encodable = {
        in match {
          case JsNull              => throw new AssertionError( s"Null input values not expected: ${JsNull}" );
          case und   : JsUndefined => throw new AssertionError( s"Undefined input values not expected: ${und}" );
          case array : JsArray     => RLP.Encodable.Seq( List( array.value.map( parseIn ) : _*) );
          case bool  : JsBoolean   => throw new AssertionError( s"Boolean input values not expected: ${bool}" );
          case num   : JsNumber    => RLP.Encodable.Int( num.value.toIntExact );
          case obj   : JsObject    => throw new AssertionError( s"Object input values not expected: ${obj}" );
          case str   : JsString    => {
            if ( str.value.length > 0 && str.value.charAt(0) == '#' )
              RLP.Encodable.BigInt( BigInt( str.value.substring(1) ) ) 
            else
              RLP.Encodable.ByteSeq( str.value.getBytes( charset ) );
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
    case class Item( name : String, in : RLP.Encodable, out : Seq[Byte] ) {
      lazy val ( encodable, rest ) = RLP.decode(out);

      def test : Boolean = RLP.encode( in ) == out

      def reverseTest : Boolean = (RLP.Encodable.sameBytes(in, encodable) && rest.length == 0);

      def verboseTest : Boolean = {
        val result = test;
        println( s"test '${name}': ${result}" );
        if (! result )
          println(s"      expected: ${ByteUtils.toHexAscii(out.toArray)}    encoded: ${ByteUtils.toHexAscii( (RLP.encode( in )).toArray )}");
        result
      }
      def verboseReverseTest : Boolean = {
        try {
          println( s"[Beginning reverse-test '$name']" );
          val result = reverseTest;
          println( s"reverse-test '${name}': ${result}" );
          if (! result )
            println(s"      expected: ${in}    decoded: ${encodable}    rest: ${rest}    sameBytes: ${RLP.Encodable.sameBytes(in, encodable)}");
          result
        } catch {
          case t : Throwable => {
            println( s"reverse-test '${name}': false" );
            println( s"      expected: ${in}    decoded: ${encodable}    rest: ${rest}    sameBytes: ${RLP.Encodable.sameBytes(in, encodable)}" );
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

def e1 = RLPSpec.EthereumRLPTest.items.forall( _.verboseTest )
def e2 = RLPSpec.EthereumRLPTest.items.forall( _.verboseReverseTest )

/*
  def is =
s2"""
   RLP should
         encode empty string to 0x80                ${ e1 }
         encode dog to 0x83646f67                   ${ e2 }
         properly encode "Lorem ipsum..."           ${ e3 }
""";

  def b( i : Int ) : Byte = i.asInstanceOf[Byte];

  val E1 = Seq( b(0x80) );
  val E2 = ByteUtils.fromHexAscii("83646f67").toSeq;
  val E3 = ByteUtils.fromHexAscii("b74c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c69").toSeq;

  def e1 : Boolean = RLP.encode( Seq.empty[Byte] ) == E1
  def e2 : Boolean = RLP.encode( "dog" ) == E2
  def e3 : Boolean = RLP.encode( "Lorem ipsum dolor sit amet, consectetur adipisicing eli" ) == E3
*/

/*
  def e2 : Boolean = {
    printEval( RLP.encode( 0x7f.asInstanceOf[Byte] ) ) match {
      case Seq( b0, b1 ) => b0 == 0x81 && b1 == 0x7f
      case _             => false
    }
  }
 */

  def printEval[T]( expr : =>T ) : T = { val out = expr; println( "===> " + out ); out }
}
