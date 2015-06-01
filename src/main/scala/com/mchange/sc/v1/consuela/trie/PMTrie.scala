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

package com.mchange.sc.v1.consuela.trie;

object PMTrie {
  class TrieException( message : String, t : Throwable = null ) extends java.lang.Exception( message, t )
  class FutureDatabaseException[H]( r : H ) extends TrieException(s"This database has been garbage collected against a future root, and may no longer contain data for root ${r}.")
  class UnknownHashException[H]( h : H, t : Throwable = null ) extends TrieException(s"Looked up but failed to find node for hash '${h}'", t);
  class DuplicateHashException[H]( h : H, t : Throwable = null) extends TrieException(s"The hash '${h}' has already been placed in the database. Cannot update immutable bindings", t);

  final object Database {
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
    self : PMTrie[_,_,H,_] =>

    def hash( node : N ) : H;
  }
}

trait PMTrie[L,V,H,I<:PMTrie[L,V,H,I]] extends Trie[L,V,I] {
  def RootHash : H;
  def EmptyHash : H;
}

