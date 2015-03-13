package com.mchange.sc.v1.consuela.trie;

object Trie {
}

trait Trie[L,V,I<:Trie[L,V,I]] {
  def alphabet                                    : IndexedSeq[L];
  def apply( key : IndexedSeq[L] )                : Option[V];
  def including( key : IndexedSeq[L], value : V ) : I;
  def excluding( key : IndexedSeq[L] )            : I;
}

