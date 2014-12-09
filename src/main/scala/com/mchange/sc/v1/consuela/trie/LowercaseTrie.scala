package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.Hash;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import scala.reflect._;

object LowercaseTrie {
  implicit val logger = MLogger( this );

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val Zero : Hash.SHA3_256 = Hash.SHA3_256.Zero;

  type Node      = BasicPMTrie.Node[Char,String,Hash.SHA3_256];
  type Branch    = BasicPMTrie.Branch[Char,String,Hash.SHA3_256];
  type Extension = BasicPMTrie.Extension[Char,String,Hash.SHA3_256];

  val Empty = BasicPMTrie.Empty;

  class MapDatabase extends BasicPMTrie.Database[Char,String,Hash.SHA3_256] {

    private[this] val _map = scala.collection.mutable.Map.empty[Hash.SHA3_256,Node];
    _map += ( LowercaseTrie.Zero -> Empty );

    private[this] val _roots = scala.collection.mutable.Set.empty[Hash.SHA3_256];

    def roots : Set[Hash.SHA3_256] = this.synchronized( _roots.toSet );
    def markRoot( root : Hash.SHA3_256 ) : Unit = this.synchronized( _roots += root );
    def knowsRoot( h : Hash.SHA3_256 ) : Boolean = this.synchronized{ _roots.contains( h) } 

    val Zero : Hash.SHA3_256                = LowercaseTrie.Zero;
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
              extension.value.foreach( dos.writeUTF(_) );
            }
            val bytes = baos.toByteArray;
            Hash.SHA3_256( bytes )
          }
        }
        case branch : Branch => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              branch.letter.foreach( dos.write( _ ) );
              branch.children.foreach( childHash => dos.write( childHash.bytes.toArray, 0, childHash.bytes.length ) );
              branch.value.foreach( dos.writeUTF(_) );
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

    def gc( roots : Set[Hash.SHA3_256], checkpoints : Set[String] = Set.empty[String] ) : Unit = ???;
    def checkpoint( name : String ) : Unit = ???;       
    def checkpoints : Set[String] = ???;
  }
}

class LowercaseTrie( val mdb : LowercaseTrie.MapDatabase = new LowercaseTrie.MapDatabase, r : Hash.SHA3_256 = LowercaseTrie.Zero ) extends {
  val earlyInit = ( mdb, r );
} with BasicPMTrie[Char,String,Hash.SHA3_256] {
  import LowercaseTrie._;

  val hashTypeClassTag : ClassTag[Hash.SHA3_256] = classTag[Hash.SHA3_256];

  val alphabet = LowercaseTrie.alphabet;

  def instantiateSuccessor( newRootHash : Hash.SHA3_256 ) : LowercaseTrie = {
    new LowercaseTrie( mdb, newRootHash );
  }
  override def excluding( key : IndexedSeq[Char] ) : LowercaseTrie = super.excluding( key ).asInstanceOf[LowercaseTrie];
  override def including( key : IndexedSeq[Char], value : String ) : LowercaseTrie = super.including( key, value ).asInstanceOf[LowercaseTrie];
}

