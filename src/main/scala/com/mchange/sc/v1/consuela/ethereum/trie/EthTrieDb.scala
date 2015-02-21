package com.mchange.sc.v1.consuela.ethereum.trie;

import com.mchange.sc.v1.consuela.trie.EmbeddableEthStyleTrie;
import com.mchange.sc.v1.consuela.ethereum.{HP, RLP, Nibble, Nibbles};

object EthTrieDb {
  val Alphabet = Nibbles;
  val AlphabetLen = Alphabet.length;

  val ExtensionLeafLength = 2;
  val BranchLength        = Nibbles.length + 1;
  val EmptyLength         = 0;

  val EmptyByteSeq             = Seq.empty[Byte];
  val RlpEncodableEmptyByteSeq = RLP.Encodable.ByteSeq( EmptyByteSeq );

  private def aerr( msg : String ) = throw new AssertionError( msg );

  object Test {
    import EmbeddableEthStyleTrie.EarlyInit;

    class Db extends EthTrieDb {
      private[this] val _map = scala.collection.mutable.Map.empty[EthHash,Node];
      _map += ( EthHash.Zero -> Empty );

      def put( hash : EthHash, node : Node ) : Unit = this.synchronized{ _map += ( hash -> node ) }
      def apply( hash : EthHash ) : Node = this.synchronized{ _map( hash ) }
    }
    class Trie( testdb : Db = new Db, rootHash : EthHash = EthHash.Zero ) extends {
      val earlyInit = EarlyInit( Alphabet, testdb, rootHash )
    } with EmbeddableEthStyleTrie[Nibble,Seq[Byte],EthHash] {
      def instantiateSuccessor( newRootHash : EthHash ) : Trie =  new Trie( testdb, newRootHash );
      override def excluding( key : Subkey ) : Trie = super.excluding( key ).asInstanceOf[Trie];
      override def including( key : Subkey, value : Seq[Byte] ) : Trie = super.including( key, value ).asInstanceOf[Trie];
    }
  }
}
trait EthTrieDb extends EmbeddableEthStyleTrie.Database[Nibble,Seq[Byte],EthHash] {
  import EthTrieDb._;

  // definitely requires access to the persistent store
  def put( hash : EthHash, node : Node ) : Unit;
  def apply( hash : EthHash ) : Node;

  // may require access to the persistent store
  def dereference( nodeSource : NodeSource ) : Node = {
    nodeSource match {
      case EmbeddableEthStyleTrie.NodeSource.Hash( hash )     => this( hash );
      case EmbeddableEthStyleTrie.NodeSource.Embedded( node ) => node;
      case EmbeddableEthStyleTrie.NodeSource.Empty            => Empty;
    }
  }

  // no access to the persistent store
  def reference( node : Node ) : NodeSource = rlpToNodeSource( toRLP( node ), Some( node ) );

  def rootReference( node : Node ) : NodeSource = { // same, a reference, but cannot be embedded, since there's nothing to embed root
    val nodeRLP = toRLP( node );
    if (nodeRLP.length == 0) NodeSource.Empty else NodeSource.Hash( EthHash.hash( nodeRLP ) )
  }
  val Zero = EthHash.Zero;

  def rlpToNodeSource( nodeRLP : Seq[Byte], mbKnownNode : Option[Node] = None ) : NodeSource = {
    if (nodeRLP.length == 0) {
      NodeSource.Empty
    } else if (nodeRLP.length < 32) {
      NodeSource.Embedded( mbKnownNode.getOrElse( fromRLP( nodeRLP ) ) );
    } else {
      NodeSource.Hash( EthHash.hash( nodeRLP ) )
    }
  }

  def completeDecode( rlpBytes : Seq[Byte] ) : RLP.Encodable = {
    val ( decoded, rest ) = RLP.decode( rlpBytes );
    assert( rest.length == 0, s"We expect to decode Byte sequences that are the result of RLP encoding. There should be no extra. rest.length -> ${rest.length}" );
    decoded
  }

  def fromRLP( rlpBytes : Seq[Byte] ) : Node = {
    def intoExtensionLeaf( decoded : Seq[RLP.Encodable] ) : Node = {
      val Seq( RLP.Encodable.ByteSeq( hpKey ), RLP.Encodable.ByteSeq( payloadBytes ) ) = decoded;
      val ( keyNibbles, terminated ) = HP.decode( hpKey );
      if ( terminated ) Leaf( keyNibbles.toIndexedSeq, payloadBytes ) else Extension( keyNibbles.toIndexedSeq, rlpToNodeSource( payloadBytes ) );
    }
    def intoBranch( decoded : Seq[RLP.Encodable] ) : Node = {
      val ( protochildren, Seq( RLP.Encodable.ByteSeq( protoMbValueBytes ) ) ) = decoded.splitAt( AlphabetLen );
      val children = protochildren.map{ encodable =>
        val RLP.Encodable.ByteSeq( childBytes ) = encodable;
        rlpToNodeSource( childBytes )  
      };

      //Note that the bytes of the value are an RLP-encoded... something
      //RLP encoding never yields an empty sequence, not even of an empty Byte string
      //So, an empty Byte string is treated as an out-of-band value signifying no value.
      val mbValue = if (protoMbValueBytes.length == 0) None else Some( protoMbValueBytes );

      Branch( children.toIndexedSeq, mbValue );
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
        case EmbeddableEthStyleTrie.NodeSource.Hash( hash )     => RLP.Encodable.ByteSeq( hash.bytes );
        case EmbeddableEthStyleTrie.NodeSource.Embedded( node ) => toEncodable( node );
        case EmbeddableEthStyleTrie.NodeSource.Empty            => RlpEncodableEmptyByteSeq
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

  /*
  def toRLP( node : Node ) = {
    val out = RLP.encode( toEncodable( node ) );
    println( s"toRLP: node -> ${node}" );
    println( s"toRLP: out -> ${out}" );
    println( s"toRLP: out (hex) -> ${out.map( com.mchange.lang.ByteUtils.toLowercaseHexAscii _ ).mkString}" );
    println( s"toRLP: out.length -> ${out.length}" );
    println( s"toRLP: RLP.decode( out ) -> ${RLP.decode( out )}" );
    out
  }

  def toRLP( node : Node ) : Seq[Byte] = {
    def branchToRLP( branch : Branch ) : Seq[Byte] = {
      val mbValueBytes = branch.mbValue.getOrElse( EmptyByteSeq );
      val branchAsByteSeqSeq : IndexedSeq[Seq[Byte]] = branch.children.map( nodeSourceToBytes _ ) :+ mbValueBytes;
      val branchSeq = RLP.Encodable.Seq( branchAsByteSeqSeq.map( RLP.Encodable.ByteSeq( _ ) ) );
      println( s"branchSeq -> $branchSeq" )
      RLP.encode( branchSeq );
    }
    def extensionToRLP( extension : Extension ) : Seq[Byte] = {
      val extSeq : Seq[RLP.Encodable] = Seq(
        RLP.Encodable.ByteSeq( HP.encode( extension.subkey, false ) ),
        RLP.Encodable.ByteSeq( nodeSourceToBytes( extension.child ) )
      );
      RLP.encode( RLP.Encodable.Seq( extSeq ) )
    }
    def leafToRLP( leaf : Leaf ) : Seq[Byte] = {
      val leafSeq : Seq[RLP.Encodable] = Seq(
        RLP.Encodable.ByteSeq( HP.encode( leaf.subkey, true ) ),
        RLP.Encodable.ByteSeq( leaf.value )
      )
      println( s"leafSeq ->${leafSeq}" )
      RLP.encode( RLP.Encodable.Seq( leafSeq ) )
    }
    def nodeSourceToBytes( nodeSource : NodeSource ) : Seq[Byte] = {
      nodeSource match {
        case EmbeddableEthStyleTrie.NodeSource.Hash( hash )     => hash.bytes;
        case EmbeddableEthStyleTrie.NodeSource.Embedded( node ) => toRLP( node );
        case EmbeddableEthStyleTrie.NodeSource.Empty            => EmptyByteSeq
      }
    }

    // finally, the method implementation
    val out = node match {
      case branch    : Branch    => branchToRLP( branch );
      case extension : Extension => extensionToRLP( extension );
      case leaf      : Leaf      => leafToRLP( leaf );
      case Empty => aerr( "Empty should hash to zero prior to and without any conversion into bytes.")
    }
    println( s"toRLP: node -> ${node}" );
    println( s"toRLP: out -> ${out}" );
    println( s"toRLP: out.length -> ${out.length}" );
    println( s"toRLP: RLP.decode( out ) -> ${RLP.decode( out )}" );
    out
  }
  */
}

