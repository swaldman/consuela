package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.EmbeddableEthStylePMTrie;
import com.mchange.sc.v1.consuela.ethereum.{HP, RLP, Nibble, Nibbles};

object EthTrieDb {
  val ExtensionLeafLength = 2;
  val BranchLength        = Nibbles.length + 1;
  val EmptyLength         = 0;

  val EmptyByteSeq        = Seq.empty[Byte];
  val EmptyHash           = EthHash.hash( RLP.Encoded.EmptyByteSeq )

  private def aerr( msg : String ) = throw new AssertionError( msg );

  object Test {
    import EmbeddableEthStylePMTrie.EarlyInit;

    class Db extends EthTrieDb {
      private[this] val _map = scala.collection.mutable.Map.empty[EthHash,Node];
      _map += ( EmptyHash -> Empty );

      def put( hash : EthHash, node : Node ) : Unit = this.synchronized{ _map += ( hash -> node ) }
      def apply( hash : EthHash ) : Node = this.synchronized{ _map( hash ) }
    }
    class Trie( testdb : Db = new Db, rootHash : EthHash = EmptyHash ) extends AbstractEthTrie[Trie]( testdb, rootHash ) {
      def instantiateSuccessor( newRootHash : EthHash ) : Trie =  new Trie( testdb, newRootHash );
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
  def EmptyHash = EthTrieDb.EmptyHash;

  def rlpToNodeSource( nodeRLP : Seq[Byte], mbKnownNode : Option[Node] = None ) : NodeSource = {
    if (nodeRLP == RLP.Encoded.EmptyByteSeq) {
      NodeSource.Empty
    } else if (nodeRLP.length < 32) {
      NodeSource.Embedded( mbKnownNode.getOrElse( fromRLP( nodeRLP ) ) );
    } else { 
      NodeSource.Hash( EthHash.hash( nodeRLP ) )
    } 
  }

  private def encodableToNodeSource( encodable : RLP.Encodable, mbKnownNode : Option[Node] = None ) : NodeSource = {
    encodable match {
      case RLP.Encodable.EmptyByteSeq                                           => NodeSource.Empty;
      case RLP.Encodable.ByteSeq( hashBytes ) if hashBytes.length == EthHashLen => NodeSource.Hash( EthHash.withBytes( hashBytes ) )
      case RLP.Encodable.Seq( Seq( keyEncodable, payloadEncodable ) ) => {
        val node = mbKnownNode.getOrElse {
          val RLP.Encodable.ByteSeq( hpKeyBytes ) = keyEncodable;
          reviveLeafExtension( hpKeyBytes, payloadEncodable )
        }
        NodeSource.Embedded( node )
      }
      case RLP.Encodable.Seq( seq ) if seq.length == AlphabetLen + 1 => {
        val node = mbKnownNode.getOrElse( intoBranch( seq )  )
        NodeSource.Embedded( node );
      }
      case _ => aerr( s"Unexpected encodable -> ${encodable}" );
    }
  }

  private def reviveLeafExtension( hpKeyBytes : Seq[Byte], payloadEncodable : RLP.Encodable ) : Node = {
    val ( keyNibbles, terminated ) = HP.decode( hpKeyBytes );
    if ( terminated ) {
      val RLP.Encodable.ByteSeq( payloadBytes ) = payloadEncodable;
      Leaf( keyNibbles.toIndexedSeq, payloadBytes )
    } else {
      Extension( keyNibbles.toIndexedSeq, encodableToNodeSource( payloadEncodable ) );
    }
  }
  private def intoBranch( decoded : Seq[RLP.Encodable] ) : Node = {
    val ( protochildren, Seq( RLP.Encodable.ByteSeq( protoMbValueBytes ) ) ) = decoded.splitAt( AlphabetLen );
    val children = protochildren.map( encodableToNodeSource( _ ) )

    //Note that the bytes of the value are an RLP-encoded... something
    //RLP encoding never yields an empty sequence, not even of an empty Byte string
    //So, an empty Byte string is treated as an out-of-band value signifying no value.
    val mbValue = if (protoMbValueBytes.length == 0) None else Some( protoMbValueBytes );

    Branch( children.toIndexedSeq, mbValue );
  }
  def fromRLP( rlpBytes : Seq[Byte] ) : Node = {
    def intoExtensionLeaf( decoded : Seq[RLP.Encodable] ) : Node = {
      val Seq( RLP.Encodable.ByteSeq( hpKey ), payloadEncodable ) = decoded;
      reviveLeafExtension( hpKey, payloadEncodable )
    }

    /* at last, the method itself */
    val RLP.Encodable.Seq( encodables ) = completeDecode( rlpBytes );
    encodables.length match {
      case ExtensionLeafLength => intoExtensionLeaf( encodables );
      case BranchLength        => intoBranch( encodables );
      case EmptyLength         => Empty;
      case _                   => aerr( "The decoded representation of an RLPed node does not match any expected length. encodables.length -> ${encodables.length}, encodables -> ${encodables}" );
    }
  }

  def toEncodable( node : Node ) : RLP.Encodable = {
    def branchToEncodable( branch : Branch ) : RLP.Encodable = {
      val mbValueBytes = branch.mbValue.getOrElse( EmptyByteSeq );
      val encodableMbValueBytes = RLP.Encodable.ByteSeq( mbValueBytes );
      val encodableChildren = branch.children.map( nodeSourceToEncodable _ );
      val fullSeq : Seq[RLP.Encodable] = encodableChildren :+ encodableMbValueBytes
      RLP.Encodable.Seq( fullSeq )
    }
    def extensionToEncodable( extension : Extension ) : RLP.Encodable = {
      val extSeq : Seq[RLP.Encodable] = Seq(
        RLP.Encodable.ByteSeq( HP.encode( extension.subkey, false ) ),
        nodeSourceToEncodable( extension.child )
      );
      RLP.Encodable.Seq( extSeq )
    }
    def leafToEncodable( leaf : Leaf ) : RLP.Encodable = {
      val leafSeq : Seq[RLP.Encodable] = Seq(
        RLP.Encodable.ByteSeq( HP.encode( leaf.subkey, true ) ),
        RLP.Encodable.ByteSeq( leaf.value )
      )
      RLP.Encodable.Seq( leafSeq )
    }
    def nodeSourceToEncodable( nodeSource : NodeSource ) : RLP.Encodable = {
      nodeSource match {
        case EmbeddableEthStylePMTrie.NodeSource.Hash( hash )     => RLP.Encodable.ByteSeq( hash.bytes );
        case EmbeddableEthStylePMTrie.NodeSource.Embedded( node ) => toEncodable( node );
        case EmbeddableEthStylePMTrie.NodeSource.Empty            => RLP.Encodable.EmptyByteSeq
      }
    }

    // finally, the method implementation
    node match {
      case branch    : Branch    => branchToEncodable( branch );
      case extension : Extension => extensionToEncodable( extension );
      case leaf      : Leaf      => leafToEncodable( leaf );
      case Empty => aerr( "Empty should hash to zero prior to and without any conversion into bytes.")
    }
  }

  def toRLP( node : Node ) : Seq[Byte] = RLP.encode( toEncodable( node ) );

  private[this] def completeDecode( rlpBytes : Seq[Byte] ) : RLP.Encodable = {
    val ( decoded, rest ) = RLP.decode( rlpBytes );
    assert( rest.length == 0, s"We expect to decode Byte sequences that are the result of RLP encoding. There should be no extra. rest.length -> ${rest.length}" );
    decoded
  }
}

