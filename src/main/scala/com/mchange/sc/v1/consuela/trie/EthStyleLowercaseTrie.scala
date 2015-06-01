/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.trie;

import com.mchange.sc.v1.consuela.hash.SHA3_256;

import EthStylePMTrie.Empty;

object EthStyleLowercaseTrie {

  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val EmptyHash : SHA3_256 = SHA3_256.Zero;

  type Node      = EthStylePMTrie.Node[Char,String,SHA3_256];
  type Branch    = EthStylePMTrie.Branch[Char,String,SHA3_256];
  type Extension = EthStylePMTrie.Extension[Char,String,SHA3_256];
  type Leaf      = EthStylePMTrie.Leaf[Char,String,SHA3_256];

  val Empty = EthStylePMTrie.Empty;

  class MapDatabase extends PMTrie.Database[Node, SHA3_256] 
      with PMTrie.Database.NodeHashing[Node,SHA3_256]
      with PMTrie.Database.RootTracking[SHA3_256] {

    private[this] val _map = scala.collection.mutable.Map.empty[SHA3_256,Node];
    _map += ( EthStyleLowercaseTrie.EmptyHash -> Empty );

    private[this] val _roots = scala.collection.mutable.Set.empty[SHA3_256];

    def roots : Set[SHA3_256] = this.synchronized( _roots.toSet );
    def markRoot( root : SHA3_256 ) : Unit = this.synchronized( _roots += root );
    def knowsRoot( h : SHA3_256 ) : Boolean = this.synchronized{ _roots.contains( h) } 

    val EmptyHash : SHA3_256                = EthStyleLowercaseTrie.EmptyHash;
    def apply( h : SHA3_256 ) : Node   = this.synchronized( _map(h) ); 
    def hash( node : Node ) : SHA3_256 = {
      import java.io._;
      import com.mchange.sc.v2.lang.borrow;

      node match {
        case Empty => EmptyHash;
        case extension : Extension => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              extension.subkey.foreach( dos.writeByte( _ ) ); // we write Chars as bytes, lowercase letters fit in one byte
              dos.write( extension.child.bytes.toArray, 0, extension.child.bytes.length );
            }
            val bytes = baos.toByteArray;
            SHA3_256.hash( bytes )
          }
        }
        case branch : Branch => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              branch.children.foreach( childHash => dos.write( childHash.bytes.toArray, 0, childHash.bytes.length ) );
              branch.mbValue.foreach( dos.writeUTF(_) );
            }
            val bytes = baos.toByteArray;
            SHA3_256.hash( bytes )
          }
        }
        case leaf : Leaf => {
          borrow( new ByteArrayOutputStream ) { baos =>
            borrow( new DataOutputStream( baos ) ) { dos =>
              leaf.subkey.foreach( dos.writeByte( _ ) ); // we write Chars as bytes, lowercase letters fit in one byte
              dos.writeUTF( leaf.value );
            }
            val bytes = baos.toByteArray;
            SHA3_256.hash( bytes )
          }
        }
      }
    }
    def put( h : SHA3_256, node : Node ) : Unit = {
      assert( h != null && node != null, s"${this} doesn't accept nulls. [ h -> ${h}, node -> ${node} ]" );
      this.synchronized {
        _map += ( h -> node );
      }
    }

    def gc( roots : Set[SHA3_256] ) : Unit = ???;
  }
}

class EthStyleLowercaseTrie( val mdb : EthStyleLowercaseTrie.MapDatabase = new EthStyleLowercaseTrie.MapDatabase, r : SHA3_256 = EthStyleLowercaseTrie.EmptyHash ) extends {
  val earlyInit = ( mdb, r );
} with EthStylePMTrie[Char,String,SHA3_256,EthStyleLowercaseTrie] {
  import EthStyleLowercaseTrie._;

  val alphabet = EthStyleLowercaseTrie.alphabet;

  def instantiateSuccessor( newRootHash : SHA3_256 ) : EthStyleLowercaseTrie = {
    new EthStyleLowercaseTrie( mdb, newRootHash );
  }
}

