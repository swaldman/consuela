package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.BasicPMTrie;
import com.mchange.sc.v1.consuela.ethereum.{HP, RLP};

trait EthTrieDb extends BasicPMTrie.Database[Nibble,Seq[Byte],EthHash] {
  val Zero = EthHash.Zero;
  def hash( node : Node ) : EthHash = if ( node == Empty ) Zero else EthHash( toBytes( node ) );

  private[this] def toBytes( node : Node ) : Array[Byte] = {
    node match {
      case branch : Branch => toBytes( branch );
      case extension : Extension => toBytes( extension );
      case Empty => throw new AssertionError( "Empty should hash to zero prior to and without any conversion into bytes.")
    }
  }
  private[this] def toBytes( branch : Branch ) : Array[Byte] = {
    ???
  }
  private[this] def toBytes( extension : Extension ) : Array[Byte] = {
    ( extension.child, extension.value ) match {
      case ( Zero, None )          => throw new AssertionError( "We should never see an extension with no child or value." );
      case ( hash, None )          => toBytesEthExtension( extension, hash );
      case ( Zero, Some( value ) ) => toBytesEthLeaf( extension, value );
      case ( hash, Some( value ) ) => toBytesEthExtendedLeaf( extension, hash, value );
    }

    /*
    if ( extension.child == Zero ) { // this should be what the Ethereum spec calls a leaf 
      toBytesEthLeaf( extension, extension.value.get ); // we assert it has a value, otherwise it shouldn't exist in the trie
    } else { // this is an Ethereum spec extension
      toBytesEthExtension( extension, child );
    }
    */ 
  }
  private[this] def toBytesEthExtendedLeaf( leaf : Extension, childHash : EthHash, value : Seq[Byte] ) : Array[Byte] = ???
  private[this] def toBytesEthLeaf( leaf : Extension, value : Seq[Byte] ) : Array[Byte] = {
    RLP.encode( RLP.Encodable.ByteSeq( HP.encode( leaf.subkey, true ) ++ value ) ).toArray
  }
  private[this] def toBytesEthExtension( leaf : Extension, childHash : EthHash ) : Array[Byte] = {
    RLP.encode( RLP.Encodable.ByteSeq( HP.encode( leaf.subkey, false ) ++ embeddedOrHash( childHash ) ) ).toArray
  }
  private[this] def embeddedOrHash( childHash : EthHash ) : Seq[Byte] = {
    val childBytes = toBytes( this.apply( childHash ) );
    if ( childBytes.length < 32 )
      childBytes
    else
      childHash.bytes;
  }
}
