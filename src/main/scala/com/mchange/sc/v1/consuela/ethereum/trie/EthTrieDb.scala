package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.EmbeddableEthStylePMTrie;
import com.mchange.sc.v1.consuela.ethereum.{EthHash, EthHashLen};
import com.mchange.sc.v1.consuela.ethereum.encoding.{Nibble, Nibbles, HP, RLP};

object EthTrieDb {
  val ExtensionLeafLength = 2;
  val BranchLength        = Nibbles.length + 1;
  val EmptyLength         = 0;

  private def aerr( msg : String ) = throw new AssertionError( msg );

  object Test {
    import EmbeddableEthStylePMTrie.EarlyInit;

    class Db extends EthTrieDb {
      private[this] val _map = scala.collection.mutable.Map.empty[EthHash,Node];
      _map += ( EmptyTrieHash -> Empty );

      def put( hash : EthHash, node : Node ) : Unit = this.synchronized{ _map += ( hash -> node ) }
      def apply( hash : EthHash ) : Node = this.synchronized{ _map( hash ) }
    }
    class Trie( testdb : Db = new Db, rootHash : EthHash = EmptyTrieHash ) extends AbstractEthTrie[Trie]( testdb, rootHash ) {
      def instantiateSuccessor( newRootHash : EthHash ) : Trie =  new Trie( testdb, newRootHash );
    }
    class SecureTrie( testdb : Db = new Db, rootHash : EthHash = EmptyTrieHash ) extends AbstractEthSecureTrie[SecureTrie]( testdb, rootHash ) {
      def instantiateSuccessor( newRootHash : EthHash ) : SecureTrie =  new SecureTrie( testdb, newRootHash );
    }
  }
}
trait EthTrieDb extends EmbeddableEthStylePMTrie.Database[Nibble,Seq[Byte],EthHash] {
  import EthTrieDb._;

  // definitely requires access to the persistent store
  def put( hash : EthHash, node : Node ) : Unit;
  def apply( hash : EthHash ) : Node;

  // may require access to the persistent store
  def dereference( nodeSource : NodeSource ) : Node = {
    nodeSource match {
      case EmbeddableEthStylePMTrie.NodeSource.Hash( hash )     => this( hash );
      case EmbeddableEthStylePMTrie.NodeSource.Embedded( node ) => node;
      case EmbeddableEthStylePMTrie.NodeSource.Empty            => Empty;
    }
  }

  // no access to the persistent store
  def reference( node : Node ) : NodeSource = rlpToNodeSource( toRLP( node ), Some( node ) );

  def rootReference( node : Node ) : NodeSource = { // same, a reference, but cannot be embedded, since there's nothing to embed root
    val nodeRLP = toRLP( node );
    if (nodeRLP.length == 0) NodeSource.Empty else NodeSource.Hash( EthHash.hash( nodeRLP ) )
  }
  def EmptyHash = EmptyTrieHash;

  def rlpToNodeSource( nodeRLP : Seq[Byte], mbKnownNode : Option[Node] = None ) : NodeSource = {
    if (nodeRLP == RLP.Encoded.EmptyByteSeq) {
      NodeSource.Empty
    } else if (nodeRLP.length < 32) {
      NodeSource.Embedded( mbKnownNode.getOrElse( fromRLP( nodeRLP ) ) );
    } else { 
      NodeSource.Hash( EthHash.hash( nodeRLP ) )
    } 
  }

  private def elementToNodeSource( element : RLP.Element, mbKnownNode : Option[Node] = None ) : NodeSource = {
    element match {
      case RLP.Element.EmptyByteSeq                                           => NodeSource.Empty;
      case RLP.Element.ByteSeq( hashBytes ) if hashBytes.length == EthHashLen => NodeSource.Hash( EthHash.withBytes( hashBytes ) )
      case RLP.Element.Seq( Seq( keyElement, payloadElement ) ) => {
        val node = mbKnownNode.getOrElse {
          val RLP.Element.ByteSeq( hpKeyBytes ) = keyElement;
          reviveLeafExtension( hpKeyBytes, payloadElement )
        }
        NodeSource.Embedded( node )
      }
      case RLP.Element.Seq( seq ) if seq.length == AlphabetLen + 1 => {
        val node = mbKnownNode.getOrElse( intoBranch( seq )  )
        NodeSource.Embedded( node );
      }
      case _ => aerr( s"Unexpected element -> ${element}" );
    }
  }

  private def reviveLeafExtension( hpKeyBytes : Seq[Byte], payloadElement : RLP.Element ) : Node = {
    val ( keyNibbles, terminated ) = HP.decode( hpKeyBytes );
    if ( terminated ) {
      val RLP.Element.ByteSeq( payloadBytes ) = payloadElement;
      Leaf( keyNibbles.toIndexedSeq, payloadBytes )
    } else {
      Extension( keyNibbles.toIndexedSeq, elementToNodeSource( payloadElement ) );
    }
  }
  private def intoBranch( decoded : Seq[RLP.Element] ) : Node = {
    val ( protochildren, Seq( RLP.Element.ByteSeq( protoMbValueBytes ) ) ) = decoded.splitAt( AlphabetLen );
    val children = protochildren.map( elementToNodeSource( _ ) )

    //Note that the bytes of the value are an RLP-encoded... something
    //RLP encoding never yields an empty sequence, not even of an empty Byte string
    //So, an empty Byte string is treated as an out-of-band value signifying no value.
    val mbValue = if (protoMbValueBytes.length == 0) None else Some( protoMbValueBytes );

    Branch( children.toIndexedSeq, mbValue );
  }
  def fromRLP( rlpBytes : Seq[Byte] ) : Node = {
    def intoExtensionLeaf( decoded : Seq[RLP.Element] ) : Node = {
      val Seq( RLP.Element.ByteSeq( hpKey ), payloadElement ) = decoded;
      reviveLeafExtension( hpKey, payloadElement )
    }

    /* at last, the method itself */
    val RLP.Element.Seq( elements ) = completeDecode( rlpBytes );
    elements.length match {
      case ExtensionLeafLength => intoExtensionLeaf( elements );
      case BranchLength        => intoBranch( elements );
      case EmptyLength         => Empty;
      case _                   => aerr( "The decoded representation of an RLPed node does not match any expected length. elements.length -> ${elements.length}, elements -> ${elements}" );
    }
  }

  def toElement( node : Node ) : RLP.Element = {
    def branchToElement( branch : Branch ) : RLP.Element = {
      val mbValueBytes = branch.mbValue.getOrElse( EmptyByteSeq );
      val elementMbValueBytes = RLP.Element.ByteSeq( mbValueBytes );
      val elementChildren = branch.children.map( nodeSourceToElement _ );
      val fullSeq : Seq[RLP.Element] = elementChildren :+ elementMbValueBytes
      RLP.Element.Seq( fullSeq )
    }
    def extensionToElement( extension : Extension ) : RLP.Element = {
      val extSeq : Seq[RLP.Element] = Seq(
        RLP.Element.ByteSeq( HP.encode( extension.subkey, false ) ),
        nodeSourceToElement( extension.child )
      );
      RLP.Element.Seq( extSeq )
    }
    def leafToElement( leaf : Leaf ) : RLP.Element = {
      val leafSeq : Seq[RLP.Element] = Seq(
        RLP.Element.ByteSeq( HP.encode( leaf.subkey, true ) ),
        RLP.Element.ByteSeq( leaf.value )
      )
      RLP.Element.Seq( leafSeq )
    }
    def nodeSourceToElement( nodeSource : NodeSource ) : RLP.Element = {
      nodeSource match {
        case EmbeddableEthStylePMTrie.NodeSource.Hash( hash )     => RLP.Element.ByteSeq( hash.bytes );
        case EmbeddableEthStylePMTrie.NodeSource.Embedded( node ) => toElement( node );
        case EmbeddableEthStylePMTrie.NodeSource.Empty            => RLP.Element.EmptyByteSeq
      }
    }

    // finally, the method implementation
    node match {
      case branch    : Branch    => branchToElement( branch );
      case extension : Extension => extensionToElement( extension );
      case leaf      : Leaf      => leafToElement( leaf );
      case Empty => aerr( "Empty should hash to zero prior to and without any conversion into bytes.")
    }
  }

  def toRLP( node : Node ) : Seq[Byte] = RLP.Element.encode( toElement( node ) );

  private[this] def completeDecode( rlpBytes : Seq[Byte] ) : RLP.Element = {
    val ( decoded, rest ) = RLP.Element.decode( rlpBytes );
    assert( rest.length == 0, s"We expect to decode Byte sequences that are the result of RLP encoding. There should be no extra. rest.length -> ${rest.length}" );
    decoded
  }
}

