/*
package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.PMTrie;
import com.mchange.sc.v1.consuela.ethereum.{HP, RLP};

trait EthTrieDb extends EmbeddableEthStyleTrie.Database[EthNode,EthHash] {
  val EmptyByteSeq = Seq.empty[Byte];

  val Zero = EthHash.Zero;
  def hash( node : Node ) : EthHash = if ( node == Empty ) Zero else EthHash( toBytes( node ) );

  private[this] def toBytes( node : Node ) : Seq[Byte] = {
    node match {
      case branch    : Branch    => toBytes( branch );
      case extension : Extension => toBytes( extension );
      case Leaf      : Leaf      => toBytes( leaf );
      case Empty => throw new AssertionError( "Empty should hash to zero prior to and without any conversion into bytes.")
    }
  }
  private[this] def toBytes( branch : Branch ) : Seq[Byte] = {
    val branchAsByteSeqSeq = branch.children.map( embeddedOrHash _ ) :+ branch.mbValue.getOrElse( EmptyByteSeq );
    val branchSeq = branchAsByteSeqSeq.map( RLP.EncodableByteSeq( _ ) );
    RLP.encode( branchSeq );
  }
  private[this] def toBytes( extension : Extension ) : Seq[Byte] = {
    val extSeq : Seq[RLP.Encodable] = Seq(
      RLP.Encodable.ByteSeq( HP.encode( extension.subkey, false ) ),
      RLP.Encodable.ByteSeq( embeddedOrHash( extension.hash ) )
    );
    RLP.encode( RLP.Encodable.Seq( extSeq ) )
  }
  private[this] def toBytes( leaf : Leaf ) : Seq[Byte] = {
    val leafSeq : Seq[RLP.Encodable] = Seq( 
      RLP.Encodable.ByteSeq( HP.encode( leaf.subkey, true ) ), 
      RLP.Encodable( leaf.value )
    )
    RLP.encode( RLP.Encodable.Seq( leafSeq ) )
  }

  private[this] def embeddedOrHash( childHash : EthHash )
    val childNode  = this.apply( childHash );
    val rawChildBytes = this.toBytes( childNode );
    val childBytes = if (rawChildBytes.length < 32) rawChildBytes else childHash.bytes;
  }

  // too complicated for now, we'd have to change a lot to support this
  private[this] def embeddedOrHashChildrenMaybeKnown( childHash : EthHash, mbKnownChildren : Option[Map[EthHash,Node]] = None ) : Seq[Byte] = {
    def fromDb( h : EthHash ) : Node = this.apply( childHash );
    val childNode  = mbKnownChildren.fold( fromDb( childHash ) ){ knownChildren => knownChildren.getOrElse( fromDb( childHash ) ) }
    val rawChildBytes = this.toBytes( childNode );
    val childBytes = if (rawChildBytes.length < 32) rawChildBytes else childHash.bytes;
  }
}

*/
