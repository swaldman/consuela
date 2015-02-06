package com.mchange.sc.v1.consuela.trie;

object Trie {
}

trait Trie[L,V] {
  def alphabet                                    : IndexedSeq[L];
  def apply( key : IndexedSeq[L] )                : Option[V];
  def including( key : IndexedSeq[L], value : V ) : Trie[L,V];
  def excluding( key : IndexedSeq[L] )            : Trie[L,V];
}

