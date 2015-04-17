package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.EmbeddableEthStylePMTrie;
import com.mchange.sc.v1.consuela.ethereum.EthHash;
import com.mchange.sc.v1.consuela.ethereum.encoding.Nibble;

abstract class AbstractEthTrie[I<:AbstractEthTrie[I]]( db : EthTrieDb, rootHash : EthHash ) extends {
  val earlyInit = EmbeddableEthStylePMTrie.EarlyInit( Alphabet, db, rootHash )
} with EmbeddableEthStylePMTrie[Nibble,Seq[Byte],EthHash,I] {
  override def including( key : IndexedSeq[Nibble], value : Seq[Byte] ) : I = {
    require( value.length > 0, "EthTries cannot store empty byte sequences as values!" );
    super.including( key, value )
  }
}
