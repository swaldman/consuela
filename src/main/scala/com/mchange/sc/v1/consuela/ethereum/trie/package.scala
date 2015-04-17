package com.mchange.sc.v1.consuela.ethereum;

import encoding.{Nibbles,RLP};

package object trie {
  val Alphabet = Nibbles;
  val AlphabetLen = Alphabet.length;

  val EmptyByteSeq        = Seq.empty[Byte];
  val EmptyTrieHash       = EthHash.hash( RLP.Encoded.EmptyByteSeq )
}

