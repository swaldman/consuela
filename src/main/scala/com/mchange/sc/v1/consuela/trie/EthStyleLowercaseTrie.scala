package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.Hash;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import EthStylePMTrie.Database;
import EthStylePMTrie.Empty;

import PMTrie.RootTracking;

object EthStyleLowercaseTrie {
  implicit val logger = MLogger( this );

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val Zero : Hash.SHA3_256 = Hash.SHA3_256.Zero;

  type Node      = EthStylePMTrie.Node[Char,String,Hash.SHA3_256];
  type Branch    = EthStylePMTrie.Branch[Char,String,Hash.SHA3_256];
  type Extension = EthStylePMTrie.Extension[Char,String,Hash.SHA3_256];
  type Leaf      = EthStylePMTrie.Leaf[Char,String,Hash.SHA3_256];

  val Empty = EthStylePMTrie.Empty;

  class MapDatabase extends Database[Char,String,Hash.SHA3_256] with RootTracking[Hash.SHA3_256] {

    private[this] val _map = scala.collection.mutable.Map.empty[Hash.SHA3_256,Node];
    _map += ( EthStyleLowercaseTrie.Zero -> Empty );

    private[this] val _roots = scala.collection.mutable.Set.empty[Hash.SHA3_256];

    def roots : Set[Hash.SHA3_256] = this.synchronized( _roots.toSet );
    def markRoot( root : Hash.SHA3_256 ) : Unit = this.synchronized( _roots += root );
    def knowsRoot( h : Hash.SHA3_256 ) : Boolean = this.synchronized{ _roots.contains( h) } 

    val Zero : Hash.SHA3_256                = EthStyleLowercaseTrie.Zero;
    def apply( h : Hash.SHA3_256 ) : Node   = this.synchronized( _map(h) ); 
    def hash( node : Node ) : Hash.SHA3_256 = {
      import java.io._;
      import com.mchange.sc.v2.lang.borrow;

      node match {
        case Empty => Zero;
        case extension : Extension => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              extension.subkey.foreach( dos.writeByte( _ ) ); // we write Chars as bytes, lowercase letters fit in one byte
              dos.write( extension.child.bytes.toArray, 0, extension.child.bytes.length );
            }
            val bytes = baos.toByteArray;
            Hash.SHA3_256( bytes )
          }
        }
        case branch : Branch => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              branch.children.foreach( childHash => dos.write( childHash.bytes.toArray, 0, childHash.bytes.length ) );
              branch.mbValue.foreach( dos.writeUTF(_) );
            }
            val bytes = baos.toByteArray;
            Hash.SHA3_256( bytes )
          }
        }
        case leaf : Leaf => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              leaf.subkey.foreach( dos.writeByte( _ ) ); // we write Chars as bytes, lowercase letters fit in one byte
              dos.writeUTF( leaf.value );
            }
            val bytes = baos.toByteArray;
            Hash.SHA3_256( bytes )
          }
        }
      }
    }
    def put( h : Hash.SHA3_256, node : Node ) : Unit = {
      assert( h != null && node != null, s"${this} doesn't accept nulls. [ h -> ${h}, node -> ${node} ]" );
      TRACE.log( s"${this} -- put( ${h}, ${node} )" );
      this.synchronized {
        _map += ( h -> node );
        TRACE.log( s"after put -- map: ${_map}" );
      }
    }

    def gc( roots : Set[Hash.SHA3_256] ) : Unit = ???;
  }
}

class EthStyleLowercaseTrie( val mdb : EthStyleLowercaseTrie.MapDatabase = new EthStyleLowercaseTrie.MapDatabase, r : Hash.SHA3_256 = EthStyleLowercaseTrie.Zero ) extends {
  val earlyInit = ( mdb, r );
} with EthStylePMTrie[Char,String,Hash.SHA3_256] {
  import EthStyleLowercaseTrie._;

  val alphabet = EthStyleLowercaseTrie.alphabet;

  def instantiateSuccessor( newRootHash : Hash.SHA3_256 ) : EthStyleLowercaseTrie = {
    new EthStyleLowercaseTrie( mdb, newRootHash );
  }
  override def excluding( key : IndexedSeq[Char] ) : EthStyleLowercaseTrie = super.excluding( key ).asInstanceOf[EthStyleLowercaseTrie];
  override def including( key : IndexedSeq[Char], value : String ) : EthStyleLowercaseTrie = super.including( key, value ).asInstanceOf[EthStyleLowercaseTrie];
}

