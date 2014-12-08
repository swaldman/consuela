package com.mchange.sc.v1.ccca.trie;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import com.mchange.sc.v1.akkonsensus.crypto.QuickCryptosystem;
import com.mchange.sc.v1.akkonsensus.crypto.QuickCryptosystem._;

import scala.reflect._;

object LowercaseTrie {
  implicit val logger = MLogger( "com.mchange.sc.v1.akkonsensus.reretrie.LowercaseTrie" );

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val Zero : QuickHash = QuickHash( IndexedSeq.fill(32)(0) );

  type Node      = BasicPMTrie.Node[Char,String,QuickHash];
  type Branch    = BasicPMTrie.Branch[Char,String,QuickHash];
  type Extension = BasicPMTrie.Extension[Char,String,QuickHash];

  val Empty = BasicPMTrie.Empty;

  class MapDatabase extends BasicPMTrie.Database[Char,String,QuickHash] {

    private[this] val _map = scala.collection.mutable.Map.empty[QuickHash,Node];
    _map += ( LowercaseTrie.Zero -> Empty );

    private[this] val _roots = scala.collection.mutable.Set.empty[QuickHash];

    def roots : Set[QuickHash] = this.synchronized( _roots.toSet );
    def markRoot( root : QuickHash ) : Unit = this.synchronized( _roots += root );
    def knowsRoot( h : QuickHash ) : Boolean = this.synchronized{ _roots.contains( h) } 

    val Zero : QuickHash                = LowercaseTrie.Zero;
    def apply( h : QuickHash ) : Node   = this.synchronized( _map(h) ); 
    def hash( node : Node ) : QuickHash = {
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
            QuickCryptosystem.hash( bytes )
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
            QuickCryptosystem.hash( bytes )
          }
        }
      }
    }
    def put( h : QuickHash, node : Node ) : Unit = {
      assert( h != null && node != null, s"${this} doesn't accept nulls. [ h -> ${h}, node -> ${node} ]" );
      TRACE.log( s"${this} -- put( ${h}, ${node} )" );
      this.synchronized {
        _map += ( h -> node );
        TRACE.log( s"after put -- map: ${_map}" );
      }
    }

    def gc( roots : Set[QuickHash], checkpoints : Set[String] = Set.empty[String] ) : Unit = ???;
    def checkpoint( name : String ) : Unit = ???;       
    def checkpoints : Set[String] = ???;
  }
}

class LowercaseTrie( val mdb : LowercaseTrie.MapDatabase = new LowercaseTrie.MapDatabase, r : QuickHash = LowercaseTrie.Zero ) extends {
  val earlyInit = ( mdb, r );
} with BasicPMTrie[Char,String,QuickHash] {
  import LowercaseTrie._;

  val hashTypeClassTag : ClassTag[QuickHash] = classTag[QuickHash];

  val alphabet = LowercaseTrie.alphabet;

  def instantiateSuccessor( newRootHash : QuickHash ) : LowercaseTrie = {
    new LowercaseTrie( mdb, newRootHash );
  }
  override def excluding( key : IndexedSeq[Char] ) : LowercaseTrie = super.excluding( key ).asInstanceOf[LowercaseTrie];
  override def including( key : IndexedSeq[Char], value : String ) : LowercaseTrie = super.including( key, value ).asInstanceOf[LowercaseTrie];
}

