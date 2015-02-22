package com.mchange.sc.v1.consuela.ethereum.trie;

import scala.io.Source;

import play.api.libs.json._;

import java.nio.charset.Charset;

import com.mchange.lang.ByteUtils;
import com.mchange.sc.v1.consuela.Implicits._
import com.mchange.sc.v1.consuela.ethereum._

import org.specs2._;

import TestUtils.UnexpectedJsValue;
import TestUtils.toStringFields;
import TestUtils.printEval;

object EthTrieDbSpec {
  object EthereumTrieAnyOrderTest {
    private[this] implicit val charset = Charset.forName("UTF-8");

    val RsrcPath : String          = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trieanyorder.json"
    lazy val JsonString : String   = Source.fromURL(this.getClass.getResource(RsrcPath)).mkString
    lazy val asJsObject : JsObject = Json.parse( JsonString ) match {
      case jso : JsObject => jso;
      case _ => throw new AssertionError("Ethereum's trieanyorder.json is expected to parse to a JsObject!");
    };
    lazy val items : Seq[Item] = EthereumTrieAnyOrderTest.asJsObject.fields.map( Item( _ ) )

    object Item {
      def keyStringToNibbles( keyStr : String ) : IndexedSeq[Nibble] = if ( keyStr.startsWith("0x") ) toNibbles( interpretHexString( keyStr ) ) else toNibbles( keyStr )
      def valStringToBytes( valStr : String ) : Seq[Byte] = if ( valStr.startsWith("0x") ) interpretHexString( valStr ) else valStr.getBytes( charset ).toSeq
      def interpretHexString( prefixedHex : String ) = ByteUtils.fromHexAscii( prefixedHex.substring(2) ).toSeq; // note we cut of the "0x" prefix
      private[this] def parseIn( in : JsValue ) : Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]] = {
        val ParseJsObject : PartialFunction[JsValue,Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]]] = {
          case obj : JsObject => {
            toStringFields( obj.fields ).map( tup => Tuple2( keyStringToNibbles( tup._1 ), valStringToBytes( tup._2 ) ) )
          }
        };
        val parser = (ParseJsObject orElse UnexpectedJsValue)
        parser( in )
      }
      private[this] def parseOut( out : String ) : Seq[Byte] = interpretHexString( out )
      def apply( field : ( String, JsValue ) ) : Item = apply( field._1, field._2 )
      def apply( name : String, jsv : JsValue ) : Item = {
        val in  = parseIn( jsv \ "in" );
        val rootHash = parseOut( (jsv \ "root").asInstanceOf[JsString].value ); 
        Item( name, in, rootHash )
      }
    }
    case class Item( name : String, in : Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]], expectedRootHash : Seq[Byte] ) {

      def test : Boolean = {
        val empty = new EthTrieDb.Test.Trie;
        val trie = in.foldLeft( empty )( ( lastTrie, pair ) => lastTrie.including( pair._1, pair._2 ) );
        val result = trie.root.bytes == expectedRootHash
        if ( !result ) {
          trie.dumpTrie
          println( s"name: ${name}, result: ${result}" )
          println( s"trie.root: ${trie.root}, expectedRootHash: ${expectedRootHash}" )
          println( s"trie.root.hex: ${trie.root.hex}, expectedRootHash.hex: ${expectedRootHash.hex}" )
        }
        result
      }
    }
  }
}
class EthTrieDbSpec extends Specification { 

  def is =
s2"""
   A Trie build on EthTrieDb should
     pass the ethereum-trieanyorder test suite             ${ e1 }
""";

  def e1 = EthTrieDbSpec.EthereumTrieAnyOrderTest.items.forall( _.test )
}
