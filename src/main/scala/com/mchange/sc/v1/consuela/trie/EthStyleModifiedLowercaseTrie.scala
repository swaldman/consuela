package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.Hash;

import EthStyleModifiedPMTrie.NodeSource;
import EthStyleModifiedPMTrie.Empty;
import EthStyleModifiedPMTrie.EarlyInit;

object EthStyleModifiedLowercaseTrie {

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val Zero : Hash.SHA3_256 = Hash.SHA3_256.Zero;

  type EarlyInit  = EthStyleModifiedPMTrie.EarlyInit[Char,String,Hash.SHA3_256,AnyRef];
  type Database   = EthStyleModifiedPMTrie.Database[Char,String,Hash.SHA3_256,AnyRef];
  type NodeSource = EthStyleModifiedPMTrie.NodeSource[Hash.SHA3_256,AnyRef];
  type Node       = EthStyleModifiedPMTrie.Node[Char,String,Hash.SHA3_256,AnyRef];
  type Branch     = EthStyleModifiedPMTrie.Branch[Char,String,Hash.SHA3_256,AnyRef];
  type Extension  = EthStyleModifiedPMTrie.Extension[Char,String,Hash.SHA3_256,AnyRef];
  type Leaf       = EthStyleModifiedPMTrie.Leaf[Char,String,Hash.SHA3_256,AnyRef];
  type Subkey     = IndexedSeq[Char];

  class MapDatabase extends EthStyleModifiedPMTrie.Database[Char,String,Hash.SHA3_256,AnyRef] {

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

class EthStyleModifiedLowercaseTrie( val mdb : EthStyleModifiedLowercaseTrie.MapDatabase = new EthStyleModifiedLowercaseTrie.MapDatabase, r : Hash.SHA3_256 = Hash.SHA3_256.Zero ) extends {
  val earlyInit = EarlyInit( EthStyleModifiedLowercaseTrie.alphabet, mdb, r );
} with EthStyleModifiedPMTrie[Char,String,Hash.SHA3_256,AnyRef] {
  import EthStyleModifiedLowercaseTrie._;

  def instantiateSuccessor( newRootHash : Hash.SHA3_256 ) : EthStyleModifiedLowercaseTrie = {
    new EthStyleModifiedLowercaseTrie( mdb, newRootHash );
  }
  override def excluding( key : Subkey ) : EthStyleModifiedLowercaseTrie = super.excluding( key ).asInstanceOf[EthStyleModifiedLowercaseTrie];
  override def including( key : Subkey, value : String ) : EthStyleModifiedLowercaseTrie = super.including( key, value ).asInstanceOf[EthStyleModifiedLowercaseTrie];
}
