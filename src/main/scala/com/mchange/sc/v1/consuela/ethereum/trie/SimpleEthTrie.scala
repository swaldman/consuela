package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.ethereum._

final class SimpleEthTrie( mydb : EthTrieDb, rootHash : EthHash ) extends AbstractEthTrie[SimpleEthTrie]( mydb, rootHash ) {
  def instantiateSuccessor( newRootHash : EthHash ) : SimpleEthTrie =  new SimpleEthTrie( mydb, newRootHash );
}
