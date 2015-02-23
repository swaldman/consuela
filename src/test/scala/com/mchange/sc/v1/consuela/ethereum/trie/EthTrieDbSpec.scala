package com.mchange.sc.v1.consuela.ethereum.trie;

import scala.io.Source;

import play.api.libs.json._;

import java.nio.charset.Charset;

import com.mchange.lang.ByteUtils;
import com.mchange.sc.v1.consuela.Implicits._
import com.mchange.sc.v1.consuela.ethereum._

import org.specs2._;

import TestUtils._;

object EthTrieDbSpec {
  private[this] implicit val charset = Charset.forName("UTF-8");

  def asJsObject( rsrcPath : String ) : JsObject = {
    val JsonString : String   = Source.fromURL(this.getClass.getResource( rsrcPath )).mkString
    Json.parse( JsonString ) match {
      case jso : JsObject => jso;
      case _ => throw new AssertionError(s"${rsrcPath} is expected to parse to a JsObject!");
    }
  }

  object EthereumTrieAnyOrderTest {
    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trieanyorder.json"
    lazy val items : Seq[Item] = asJsObject( RsrcPath ).fields.map( Item( _ ) )
    lazy val test = items.forall( _.test );
  }
  object EthereumTrieTest {
    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trietest.json"
    lazy val items : Seq[Item] = asJsObject( RsrcPath ).fields.map( Item( _ ) )
    lazy val test = items.forall( _.test );
  }
  object EthereumTrieTestNextPrev {
    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trietestnextprev.json"
    lazy val items : Seq[Item] = asJsObject( RsrcPath ).fields.map( Item( _ ) )
    lazy val test = items.forall( _.test );
  }

  object Item {
    def keyStringToNibbles( keyStr : String ) : IndexedSeq[Nibble] = if ( keyStr.startsWith("0x") ) toNibbles( interpretHexString( keyStr ) ) else toNibbles( keyStr )
    def valStringToBytes( valStr : String ) : Seq[Byte] = {
      if ( valStr == null ) null else if ( valStr.startsWith("0x") ) interpretHexString( valStr ) else valStr.getBytes( charset ).toSeq
    }
    def interpretHexString( prefixedHex : String ) = ByteUtils.fromHexAscii( prefixedHex.substring(2) ).toSeq; // note we cut of the "0x" prefix
    private[this] def parseIn( in : JsValue ) : Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]] = {
      val ParseJsObjectOrArray : PartialFunction[JsValue,Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]]] = {
        case obj : JsObject => {
          toNullableStringFields( obj.fields ).map( tup => Tuple2( keyStringToNibbles( tup._1 ), valStringToBytes( tup._2 ) ) )
        }
        case arr : JsArray => {
          def pairArrayToTuple( pairArray : JsArray ) = {
            val nullableStringPairArray = pairArray.value.map( toStringOrNull( _ ) )
            Tuple2( keyStringToNibbles( nullableStringPairArray(0) ), valStringToBytes( nullableStringPairArray(1) ) )
          }
          arr.value.map {
            case pairArray : JsArray => pairArrayToTuple( pairArray );
            case whatever => throw new AssertionError( s"Unexpected value in JsArray: ${whatever}" );
          }
        }
      };
      val parser = (ParseJsObjectOrArray orElse UnexpectedJsValue)
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
      val nextTrie : ( EthTrieDb.Test.Trie, Tuple2[IndexedSeq[Nibble],Seq[Byte]] ) => EthTrieDb.Test.Trie = { ( lastTrie, pair ) =>
        if ( pair._2 == null) lastTrie.excluding( pair._1 ) else lastTrie.including( pair._1, pair._2 )
      }
      val trie = in.foldLeft( empty )( nextTrie );
      val result = trie.RootHash.bytes == expectedRootHash
      if ( !result ) {
        trie.dumpTrie
        println( s"name: ${name}, result: ${result}" )
        println( s"trie.RootHash: ${trie.RootHash}, expectedRootHash: ${expectedRootHash}" )
        println( s"trie.RootHash.hex: ${trie.RootHash.hex}, expectedRootHash.hex: ${expectedRootHash.hex}" )
      }
      else {
        println( s"passed: ${name}" )
      }
      result
    }
  }
}
class EthTrieDbSpec extends Specification { 
  import EthTrieDbSpec._

  def is =
s2"""
   A Trie build on EthTrieDb should
     pass the ethereum-trietestanyorder test suite             ${ e1 }
     pass the ethereum-trietest         test suite             ${ e2 }
""";
//     pass the ethereum-trienextprev test suite             ${ e3 }

  def e1 = EthereumTrieAnyOrderTest.test
  def e2 = EthereumTrieTest.test
//  def e3 = EthereumTrieTestNextPrev.test
}
