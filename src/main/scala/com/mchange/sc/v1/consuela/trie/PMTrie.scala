package com.mchange.sc.v1.consuela.trie;

trait PMTrie[L,V,H,N] extends Trie[L,V] {
  def root : H;
  def Zero : H;
  def hash( node : N ) : H;
}

