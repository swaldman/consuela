package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.Hash;

import EmbeddableEthStylePMTrie.NodeSource;
import EmbeddableEthStylePMTrie.Empty;
import EmbeddableEthStylePMTrie.EarlyInit;

object EmbeddableEthStyleLowercaseTrie {

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val Zero : Hash.SHA3_256 = Hash.SHA3_256.Zero;

  type EarlyInit  = EmbeddableEthStylePMTrie.EarlyInit[Char,String,Hash.SHA3_256,AnyRef];
  type Database   = EmbeddableEthStylePMTrie.Database[Char,String,Hash.SHA3_256,AnyRef];
  type NodeSource = EmbeddableEthStylePMTrie.NodeSource[Hash.SHA3_256,AnyRef];
  type Node       = EmbeddableEthStylePMTrie.Node[Char,String,Hash.SHA3_256,AnyRef];
  type Branch     = EmbeddableEthStylePMTrie.Branch[Char,String,Hash.SHA3_256,AnyRef];
  type Extension  = EmbeddableEthStylePMTrie.Extension[Char,String,Hash.SHA3_256,AnyRef];
  type Leaf       = EmbeddableEthStylePMTrie.Leaf[Char,String,Hash.SHA3_256,AnyRef];
  type Subkey     = IndexedSeq[Char];

  class MapDatabase extends EmbeddableEthStylePMTrie.Database[Char,String,Hash.SHA3_256,AnyRef] {

    private[this] val _map = scala.collection.mutable.Map.empty[Hash.SHA3_256,Node];
    _map += ( Hash.SHA3_256.Zero -> Empty );

    def put( hash : Hash.SHA3_256, node : Node ) : Unit = this.synchronized{ _map += Tuple2( hash, node ) }
    def apply( hash : Hash.SHA3_256 ) : Node = this.synchronized{ _map(hash) }; 

    def dereference( source : NodeSource ) : Node = {
      source match {
        case NodeSource.Hash( hash ) => this( hash );
        case NodeSource.Embedded( node ) => node.asInstanceOf[Node]; 
        case NodeSource.Empty => Empty;
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

    val Zero : Hash.SHA3_256 = Hash.SHA3_256.Zero;

    private[this] def hash( node : Node ) : Hash.SHA3_256 = {
      import java.io._;
      import com.mchange.sc.v2.lang.borrow;

      def b( byte : Byte ) : Byte = byte; // force coercion of Int to Byte
      def subkeyToBytes( subkey : Subkey ) : Seq[Byte] = subkey.map( c=> ((c & 0xFF).toByte) );
      def nseToBytes( nse : NodeSource.Embedded[AnyRef] ) : Seq[Byte] = nodeToBytes( nse.embedded.asInstanceOf[Node] );
      def nshToBytes( nsh : NodeSource.Hash[Hash.SHA3_256] ) : Seq[Byte] = nsh.hash.bytes;
      def strToBytes( str : String ) : Seq[Byte] = str.getBytes("UTF-8").toSeq;
      def osToBytes( mbStr : Option[String] ) : Seq[Byte] = mbStr.fold( Seq( b(0) ) )( str => (b(0) +: str.getBytes("UTF-8")).toSeq :+ b(0) );
      def nsToBytes( ns  : NodeSource ) : Seq[Byte] = (ns : @unchecked) match {
        case nse : NodeSource.Embedded[AnyRef] => nseToBytes( nse );
        case nsh : NodeSource.Hash[Hash.SHA3_256] => nshToBytes( nsh );
        case NodeSource.Empty => Seq.empty[Byte]
      }
      def nodeToBytes( node : Node ) : Seq[Byte] = {
        node match {
          case Empty => Zero.bytes;
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
      Hash.SHA3_256( nodeToBytes( node ) )
    }
  }
}

class EmbeddableEthStyleLowercaseTrie( val mdb : EmbeddableEthStyleLowercaseTrie.MapDatabase = new EmbeddableEthStyleLowercaseTrie.MapDatabase, r : Hash.SHA3_256 = Hash.SHA3_256.Zero ) extends {
  val earlyInit = EarlyInit( EmbeddableEthStyleLowercaseTrie.alphabet, mdb, r );
} with EmbeddableEthStylePMTrie[Char,String,Hash.SHA3_256,AnyRef] {
  import EmbeddableEthStyleLowercaseTrie._;

  def instantiateSuccessor( newRootHash : Hash.SHA3_256 ) : EmbeddableEthStyleLowercaseTrie = {
    new EmbeddableEthStyleLowercaseTrie( mdb, newRootHash );
  }
  override def excluding( key : Subkey ) : EmbeddableEthStyleLowercaseTrie = super.excluding( key ).asInstanceOf[EmbeddableEthStyleLowercaseTrie];
  override def including( key : Subkey, value : String ) : EmbeddableEthStyleLowercaseTrie = super.including( key, value ).asInstanceOf[EmbeddableEthStyleLowercaseTrie];
}
