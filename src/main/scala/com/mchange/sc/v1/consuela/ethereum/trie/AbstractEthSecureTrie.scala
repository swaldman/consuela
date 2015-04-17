package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.encoding._;

abstract class AbstractEthSecureTrie[I<:AbstractEthSecureTrie[I]]( db : EthTrieDb, rootHash : EthHash ) extends AbstractEthTrie[I]( db, rootHash ) {
  def xformKey( key : IndexedSeq[Nibble] ) : IndexedSeq[Nibble] = toNibbles( EthHash.hash( nibblesToBytes( key ) ).bytes );
  override def including( key : IndexedSeq[Nibble], value : Seq[Byte] ) : I = super.including( xformKey( key ), value );
  override def excluding( key : IndexedSeq[Nibble] ) : I = super.excluding( xformKey( key ) );
}

