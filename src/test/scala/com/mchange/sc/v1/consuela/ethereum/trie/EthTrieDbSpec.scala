/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum.trie;

import scala.io.Source;
import scala.util.Random;

import play.api.libs.json._;

import java.nio.charset.Charset;

import com.mchange.lang.ByteUtils;
import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._

import org.specs2._;

import TestUtils._;

object EthTrieDbSpec {

  private implicit val charset = Charset.forName("UTF-8");
  private val NumPermutations = 1000;

  def asJsObject( rsrcPath : String ) : JsObject = {
    val JsonString : String   = Source.fromURL(this.getClass.getResource( rsrcPath )).mkString
    Json.parse( JsonString ) match {
      case jso : JsObject => jso;
      case _ => throw new AssertionError(s"${rsrcPath} is expected to parse to a JsObject!");
    }
  }

  trait Testable {
    val RsrcPath : String;

    lazy val items : Seq[Item] = asJsObject( RsrcPath ).fields.map( Item( _ ) )

    def test = items.forall( _.test );
  }

  /**
   * Following ethereum sources ( e.g https://github.com/ethereum/pyethereum/blob/d442c114b694439a85a89bb3b06d5115243d3067/tests/test_trie.py )
   * it looks like some Trie tests are intended to be permuted, but with all deletions deterministic tacked onto the end to keep the outcome hash
   * deterministic despite keys both added and removed.
   * 
   * So we support this sort of permutation testing. But some tests cannot be permuted this way. For example, they may have multiple nondelete
   * bindings, so that the final trie depends on which came last. For now, we exclude those tests by hand from permutation testing.
   *
   * See https://github.com/ethereum/tests/issues/64 
   */ 
  trait PermuteTestable extends Testable {
    val unpermutableTests : Set[String] = Set.empty;

    def permuteTest = {
      val random = new Random;
      items.filter( item => !unpermutableTests( item.name ) ).forall { item =>
        (1 to NumPermutations).forall { num => item.testPermutation( num, random ) }
      }
    }
  }

  object EthereumTrieAnyOrderTest extends PermuteTestable {
    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trieanyorder.json"
  }
  object EthereumTrieTest extends PermuteTestable {
    override val unpermutableTests = Set( "jeff" )

    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trietest.json"
  }
  object EthereumTrieTestNextPrev extends Testable {
    val RsrcPath : String      = "/com/mchange/sc/v1/consuela/ethereum/ethereum-tests-trietestnextprev.json"
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
      val in  = parseIn( (jsv \ "in").get );                                          // asserts existence of jsv / "in"
      val rootHash = parseOut( ( (jsv \ "root").get ).asInstanceOf[JsString].value ); // asserts existence of jsv / "root"
      Item( name, in, rootHash )
    }
  }
  case class Item( name : String, in : Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]], expectedRootHash : Seq[Byte] ) {

    val verbose = false;

    lazy val deletes = in.filter( _._2 == null )

    def testPermutation( num : Int, random : Random ) : Boolean = {
      val shuffled = random.shuffle( in );
      val operations = shuffled ++ deletes;
      doTest( operations, Some( num ) ); 
    }

    def test = doTest( in, None );

    private[this] def doTest( operationTuples : Seq[Tuple2[IndexedSeq[Nibble],Seq[Byte]]], permutationNumber : Option[Int] = None ) : Boolean = {
      val empty = new EthTrieDb.Test.Trie;
      val nextTrie : ( EthTrieDb.Test.Trie, Tuple2[IndexedSeq[Nibble],Seq[Byte]] ) => EthTrieDb.Test.Trie = { ( lastTrie, pair ) =>
        if ( pair._2 == null) lastTrie.excluding( pair._1 ) else lastTrie.including( pair._1, pair._2 )
      }
      val trie = operationTuples.foldLeft( empty )( nextTrie );
      val result = trie.RootHash.bytes == expectedRootHash
      val permuted = permutationNumber != None;
      if ( !result ) {
        def isPermutedStr = permutationNumber.fold("")( num => s" (permutation #${num})" )
        if ( permuted ) {
          println("unpermuted:");
          in.foldLeft( empty )( nextTrie ).dumpTrie;
          println();
          println("permuted:");
        } 
        trie.dumpTrie;
        println();
        println( s"""name: ${name}${isPermutedStr}, result: ${result}""" )
        println( s"trie.RootHash: ${trie.RootHash}, expectedRootHash: ${expectedRootHash}" )
        println( s"trie.RootHash.hex: ${trie.RootHash.hex}, expectedRootHash.hex: ${expectedRootHash.hex}" )
      }
      else {
        if (verbose && !permuted)
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
     pass the ethereum-trietestanyorder           test suite                     ${ e1 }
     pass the permuting ethereum-trietestanyorder test suite                     ${ e2 }
     pass the simple ethereum-trietest            test suite                     ${ e3 }
     pass the permuting ethereum-trietest         test suite (excluding 'jeff')  ${ e4 }
  
   Note: see https://github.com/ethereum/tests/issues/64 re excluded tests.
""";
//     pass the ethereum-trienextprev test suite             ${ e3 }

  def e1 = EthereumTrieAnyOrderTest.test
  def e2 = EthereumTrieAnyOrderTest.permuteTest
  def e3 = EthereumTrieTest.test
  def e4 = EthereumTrieTest.permuteTest
//  def e3 = EthereumTrieTestNextPrev.test
}
