package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.SHA3_256;

import EmbeddableEthStylePMTrie.EarlyInit;

object EmbeddableEthStyleLowercaseTrie {
  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val EmptyHash : SHA3_256 = SHA3_256.Zero;

  class MapDatabase extends EmbeddableEthStylePMTrie.Database[Char,String,SHA3_256] {

    import EmbeddableEthStylePMTrie.NodeSource;

    private[this] val _map = scala.collection.mutable.Map.empty[SHA3_256,Node];
    _map += ( EmptyHash -> Empty );

    def put( hash : SHA3_256, node : Node ) : Unit = this.synchronized{ _map += Tuple2( hash, node ) }
    def apply( hash : SHA3_256 ) : Node = this.synchronized{ _map(hash) }; 

    def dereference( source : NodeSource ) : Node = {
      source match {
        case EmbeddableEthStylePMTrie.NodeSource.Hash( hash ) => this( hash );
        case EmbeddableEthStylePMTrie.NodeSource.Embedded( node ) => node
        case EmbeddableEthStylePMTrie.NodeSource.Empty => Empty;
        case _ => throw new AssertionError( s"Dereferencing unexpected source: ${source}" );
      }
    }
    def reference( node : Node )     : NodeSource = {
      node match {
        case leaf : Leaf => NodeSource.Embedded( node );
        case _ => NodeSource.Hash( hash( node ) );
      }
    }
    def rootReference( node : Node ) : NodeSource = if (node == Empty) NodeSource.Empty else NodeSource.Hash( hash( node ) );

    def EmptyHash : SHA3_256 = EmbeddableEthStyleLowercaseTrie.EmptyHash

    private[this] def hash( node : Node ) : SHA3_256 = {
      import java.io._;
      import com.mchange.sc.v2.lang.borrow;

      def b( byte : Byte ) : Byte = byte; // force coercion of Int to Byte
      def subkeyToBytes( subkey : Subkey ) : Seq[Byte] = subkey.map( c=> ((c & 0xFF).toByte) );
      def nseToBytes( nse : NodeSource.Embedded[Char,String,SHA3_256] ) : Seq[Byte] = nodeToBytes( nse.node );
      def nshToBytes( nsh : NodeSource.Hash[Char,String,SHA3_256] ) : Seq[Byte] = nsh.hash.bytes;
      def strToBytes( str : String ) : Seq[Byte] = str.getBytes("UTF-8").toSeq;
      def osToBytes( mbStr : Option[String] ) : Seq[Byte] = mbStr.fold( Seq( b(0) ) )( str => (b(0) +: str.getBytes("UTF-8")).toSeq :+ b(0) );
      def nsToBytes( ns  : NodeSource ) : Seq[Byte] = (ns : @unchecked) match {
        case nse : EmbeddableEthStylePMTrie.NodeSource.Embedded[Char,String,SHA3_256] => nseToBytes( nse );
        case nsh : EmbeddableEthStylePMTrie.NodeSource.Hash[Char,String,SHA3_256] => nshToBytes( nsh );
        case EmbeddableEthStylePMTrie.NodeSource.Empty => Seq.empty[Byte]
      }
      def nodeToBytes( node : Node ) : Seq[Byte] = {
        node match {
          case Empty => EmptyHash.bytes;
          case extension : Extension => {
            borrow( new ByteArrayOutputStream ) { baos =>
              baos.write( subkeyToBytes( extension.subkey ).toArray );
              baos.write( nsToBytes( extension.child ).toArray );
              baos.toByteArray
            }
          }
          case branch : Branch => {
            borrow( new ByteArrayOutputStream ) { baos =>
              branch.children.foreach( ns => baos.write( nsToBytes( ns ).toArray ) );
              baos.write( osToBytes( branch.mbValue ).toArray );
              baos.toByteArray
            }
          }
          case leaf : Leaf => {
            borrow( new ByteArrayOutputStream ) { baos =>
              baos.write( subkeyToBytes(leaf.subkey).toArray );
              baos.write( strToBytes(leaf.value).toArray );
              baos.toByteArray
            }
          }
        }
      }
      SHA3_256.hash( nodeToBytes( node ) )
    }
  }
}

class EmbeddableEthStyleLowercaseTrie( 
  mdb : EmbeddableEthStyleLowercaseTrie.MapDatabase = new EmbeddableEthStyleLowercaseTrie.MapDatabase, 
  r : SHA3_256 = EmbeddableEthStyleLowercaseTrie.EmptyHash 
) extends {
  val earlyInit = EarlyInit( EmbeddableEthStyleLowercaseTrie.alphabet, mdb, r );
} with EmbeddableEthStylePMTrie[Char,String,SHA3_256,EmbeddableEthStyleLowercaseTrie] {
  import EmbeddableEthStyleLowercaseTrie._;

  def instantiateSuccessor( newRootHash : SHA3_256 ) : EmbeddableEthStyleLowercaseTrie = new EmbeddableEthStyleLowercaseTrie( mdb, newRootHash );
}

