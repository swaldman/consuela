package com.mchange.sc.v1.consuela.ethereum;

import scala.io.Source;

import play.api.libs.json._;

import com.mchange.lang.ByteUtils;

import org.specs2._;

object HPSpec {
  object EthereumHexencodeTest {
    val RsrcPath : String = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-hexencodetest.json";

    lazy val JsonString : String   = Source.fromURL(this.getClass.getResource(RsrcPath)).mkString
    lazy val asJsObject : JsObject = Json.parse( JsonString ) match {
      case jso : JsObject => jso;
      case _ => throw new AssertionError("Ethereum's hexencodetest.json is expected to parse to a JsObject!");
    };
    lazy val items : Seq[Item] = EthereumHexencodeTest.asJsObject.fields.map( Item( _ ) )
  }
  object Item {
    /*
     * "seq" is interpreted as a Sequence of nibbles
     * "term" is "flag" in our HP implementation, a Boolean
     * "out" we read from String as a sequence of hex ascii bytes
     */ 
    case class Elements( seq : Seq[Int], term : Boolean, out : Seq[Byte] );
    def apply( pair : ( String, JsValue ) ) : Item = apply( pair._1, pair._2 )
    def apply( name : String, info : JsValue ) : Item = {
      info match {
        case jso : JsObject => apply( name, jso );
        case _ => throw new AssertionError("Items in Ethereum's hexencodetest.json are expected to parse to a JsObject!");
      }
    }
    def apply( name : String, info : JsObject ) : Item = {
      val rawSeq : JsValue = info \ "seq";
      val rawTerm : JsValue = info \ "term";
      val rawOut : JsValue = info \ "out";

      def unrawSeq( value : JsValue ) : Seq[Int] = value.asInstanceOf[JsArray].value.map( _.asInstanceOf[JsNumber].value.toIntExact )
      def unrawTerm( value : JsValue ) : Boolean = value.asInstanceOf[JsBoolean].value
      def unrawOut( value : JsValue ) : Seq[Byte] = ByteUtils.fromHexAscii( value.asInstanceOf[JsString].value ).toSeq

      Item( name, Elements( unrawSeq( rawSeq ), unrawTerm( rawTerm ), unrawOut( rawOut ) ) )
    }
  }
  case class Item( name : String, elements : Item.Elements ) {
    lazy val test : Boolean = HP.encode( elements.seq, elements.term ) == elements.out
    lazy val reverseTest : Boolean = HP.decode( elements.out ) == ( elements.seq, elements.term )

    def verboseTest : Boolean = {
      val result = test;
      println( s"test '${name}': ${result}" );
      if (! result )
        println(s"      expected: ${ByteUtils.toHexAscii(elements.out.toArray)}    encoded: ${ByteUtils.toHexAscii( (HP.encode( elements.seq, elements.term )).toArray )}");
      result
    }
  }
}
class HPSpec extends Specification { 

  def is =
s2"""
   HP should
     pass the ethereum hexencode test suite             ${ e1 }
     pass the ethereum hexencode test suite in reverse  ${ e2 }
""";

  def e1 = HPSpec.EthereumHexencodeTest.items.forall( /* _.verboseTest */ _.test )
  def e2 = HPSpec.EthereumHexencodeTest.items.forall( /* _.verboseReverseTest */ _.reverseTest )

}
