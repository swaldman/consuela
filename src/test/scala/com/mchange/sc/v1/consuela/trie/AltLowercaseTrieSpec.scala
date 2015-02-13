package com.mchange.sc.v1.consuela.trie;

import org.specs2._;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

class AltLowercaseTrieSpec extends Specification { 

  private[this] implicit val logger = MLogger( this );

  def is =
s2"""
   An AltLowercaseTrie can
         return "world" after it has stored the binding "hello" -> "world"                        ${ e1 }
         return "said" after it has stored the binding "he" -> "said" after "hello" -> "world"    ${ e2 }
         return to an EmptyNode root when all bindings are deleted                                ${ e3 }
         create a branch where divergent keys share the same prefix                               ${ e4 } 
         create a tree with a branch value                                                        ${ e5 } 
         restore a branched tree into a linear tree                                               ${ e6 } 
         create a trie with five-child branch value                                               ${ e7 } 
         create a trie with two adjacent branches                                                 ${ e8 } 
         restore a complex trie with adjacent branches to a single linear Extension               ${ e9 } 
         restore linear extension to empty tree                                                   ${ eA } 
         branch at root                                                                           ${ eB } 
         empty string at root                                                                     ${ eC } 
         empty string at root plus extension binding                                              ${ eD } 
         empty string at root plus extension split                                                ${ eE } 
         empty string at root plus extension rearranged to branch                                 ${ eF } 
         remove empty string at root                                                              ${ eG } 
         empty string at root plus extension rearranged to branch restored to 2 linear extensions ${ eH } 
         previous condensed to one extension by removing root node value                          ${ eI } 
         root extension rearranged to branch                                                      ${ eJ }
         root branch deleted down to empty                                                        ${ eK }
         consense new element into truncated path and not blow up                                 ${ eL }

""";

  val tt0 = new AltLowercaseTrie();
  val tt1 = tt0.including( "hello", "world" );
  val tt2 = tt1.including("he","said");
  val tt3 = tt1.excluding( "hello" );
  val tt4 = tt1.including( "heather", "hitch" );
  val tt5 = tt4.including( "he", "said" );
  val tt6 = tt5.excluding( "hello" );
  val tt7 = tt5.including( "hexadsup", "catsup" ).including( "heyathen", "breathen" )
  val tt8 = tt5.including( "headsup", "catsup" ).including( "heathen", "breathen" )
  val tt9 = tt8.excluding( "heathen" ).excluding( "he" ).excluding( "heather" ).excluding( "hello" );
  val ttA = tt9.excluding( "headsup" );
  val ttB = tt8.including( "book", "nook" );
  val ttC = tt0.including( "", "Yay!" );
  val ttD = ttC.including( "hello", "world" );
  val ttE = ttD.including("he","said");
  val ttF = ttE.including("book","nook");
  val ttG = ttC.excluding("");
  val ttH = ttF.excluding("he").excluding("hello");
  val ttI = ttH.excluding("");
  val ttJ = ttI.including("hi", "again");
  val ttK = ttJ.excluding("hi").excluding("book");

  /*
  println("tt0:");
  tt0.dumpTrie;
  println(); println();
  println("tt1:");
  tt1.dumpTrie;
  println(); println();
  println("tt2:");
  tt2.dumpTrie;
  println(); println();
  println("tt3:");
  tt3.dumpTrie;
  println(); println();
  println("tt4:");
  tt4.dumpTrie;
  println(); println();
  println("tt5:");
  tt5.dumpTrie;
  println(); println();
  println("tt6:");
  tt6.dumpTrie;
  println(); println();
  println("tt7:");
  tt7.dumpTrie;
  println(); println();
  println("tt8:");
  tt8.dumpTrie;
  println(); println();
  println("tt9:");
  tt9.dumpTrie;
  println(); println();
  println("ttA:");
  ttA.dumpTrie;
  println(); println();
  println("ttB:");
  ttB.dumpTrie;
  println(); println();
  println("ttC:");
  ttC.dumpTrie;
  println(); println();
  println("ttD:");
  ttD.dumpTrie;
  println(); println();
  println("ttE:");
  ttE.dumpTrie;
  println(); println();
  ttF.dumpTrie;
  println(); println();
  ttG.dumpTrie;
  println(); println();
  ttH.dumpTrie;
  println(); println();
  ttI.dumpTrie;
  println(); println();
  ttJ.dumpTrie;
  println(); println();
  ttK.dumpTrie;
  println(); println();
  */ 


  val testBindings = Map(
    "hello" -> "world",
    "he" -> "said",
    "heather" -> "hitch",
    "hexadsup" -> "catsup",
    "heyathen" -> "breathen",
    "headsup" -> "catsup",
    "heathen" -> "breathen",
    "book" -> "nook",
    "" -> "Yay!",
    "hi" -> "again"
  )

  def checkTrie( trie : AltLowercaseTrie, yes : Set[String] ) : Boolean = {
    testBindings.filter( binding => yes(binding._1) ).forall( binding => trie( binding._1 ).contains( binding._2 ) ) && 
    testBindings.filter( binding => !yes(binding._1) ).forall( binding => trie( binding._1 ) == None )
  }
  def checkEmpty( trie : AltLowercaseTrie ) = tt3.root == tt3.db.Zero;


  def e1 : Boolean = checkTrie( tt1, Set("hello") ) 
  def e2 : Boolean = checkTrie( tt2, Set("hello","he") ) 
  def e3 : Boolean = checkEmpty( tt3 );
  def e4 : Boolean = checkTrie( tt4, Set("hello","heather") )
  def e5 : Boolean = checkTrie( tt5, Set("hello", "heather", "he") )
  def e6 : Boolean = checkTrie( tt6, Set("heather", "he") )
  def e7 : Boolean = checkTrie( tt7, Set("hello", "heather", "he", "hexadsup", "heyathen") )
  def e8 : Boolean = checkTrie( tt8, Set("hello", "heather", "he", "headsup", "heathen") )
  def e9 : Boolean = checkTrie( tt9, Set("headsup") )
  def eA : Boolean = checkEmpty( ttA )
  def eB : Boolean = checkTrie( ttB, Set("hello", "heather", "he", "headsup", "heathen", "book") )
  def eC : Boolean = checkTrie( ttC, Set("") )
  def eD : Boolean = checkTrie( ttD, Set("","hello") )
  def eE : Boolean = checkTrie( ttE, Set("","hello", "he") )
  def eF : Boolean = checkTrie( ttF, Set("","hello", "he", "book") )
  def eG : Boolean = checkEmpty( ttG )
  def eH : Boolean = checkTrie( ttH, Set("", "book") )
  def eI : Boolean = checkTrie( ttI, Set("book") )
  def eJ : Boolean = checkTrie( ttJ, Set("book","hi") )
  def eK : Boolean = checkEmpty( ttK )

  def eL : Boolean = {
    tt0.including("dog","puppy").including("horse","stallion").including("do","verb").including("doge","coin") /*.dumpTrie*/
    true;
  }

}

