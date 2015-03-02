package com.mchange.sc.v1.consuela.trie;

object PMTrie {
  class TrieException( message : String, t : Throwable = null ) extends java.lang.Exception( message, t )
  class FutureDatabaseException[H]( r : H ) extends TrieException(s"This database has been garbage collected against a future root, and may no longer contain data for root ${r}.")
  class UnknownHashException[H]( h : H, t : Throwable = null ) extends TrieException(s"Looked up but failed to find node for hash '${h}'", t);
  class DuplicateHashException[H]( h : H, t : Throwable = null) extends TrieException(s"The hash '${h}' has already been placed in the database. Cannot update immutable bindings", t);

  object Database {
    trait NodeHashing[N,H] {
      self : Database[N,H] =>

      def hash( node : N ) : H;
    }
    trait GarbageCollecting[H] {
      self : Database[_, H] =>

      def gc( roots : Set[H] ) : Unit;
    }
    trait RootTracking[H] {
      self : Database[_, H] =>

      def roots : Set[H];
      def markRoot( root : H ) : Unit;
      def knowsRoot( h : H ) : Boolean;
    }
    trait Savepointing[H] {
      self : Database[_, H] =>

      def savepoint( name : String ) : Unit;
      def savepoints : Set[String];

      def gc( roots : Set[H], savepoints : Set[String] ) : Unit;
    }
    trait BulkWriting[N,H] {
      self : Database[N,H] =>

      def put( nodes : Map[H,N] ) : Unit;
    }
  }
  trait Database[N,H] {
    // a lookup on the hash's Empty value should always return a node representing an empty Trie
    def EmptyHash : H;

    def apply( h : H ) : N;       // throws UnknownHashException              
    def put( h : H, node : N ) : Unit;
  }

  // TODO: Trait HistoryTracking or LineTracking where roots are annotated by 
  //       successive hashes of past roots to distinguish identical trees with
  //       different histories.


  trait Regular[N,H] { // A "Regular" PMTrie has a hash for every node, i.e. no embedding
    self : PMTrie[_,_,H] =>

    def hash( node : N ) : H;
  }
}

trait PMTrie[L,V,H] extends Trie[L,V] {
  def RootHash : H;
  def EmptyHash : H;
}

