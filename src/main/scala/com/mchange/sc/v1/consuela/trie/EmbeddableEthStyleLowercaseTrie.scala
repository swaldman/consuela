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

import com.mchange.sc.v1.consuela.hash.Keccak256;

import EmbeddableEthStylePMTrie.EarlyInit;

object EmbeddableEthStyleLowercaseTrie {
  val alphabet : IndexedSeq[Char] = IndexedSeq( 'a' to 'z' : _* );
  val EmptyHash : Keccak256 = Keccak256.Zero;

  class MapDatabase extends EmbeddableEthStylePMTrie.Database[Char,String,Keccak256] {

    import EmbeddableEthStylePMTrie.NodeSource;

    private[this] val _map = scala.collection.mutable.Map.empty[Keccak256,Node];
    _map += ( EmptyHash -> Empty );

    def put( hash : Keccak256, node : Node ) : Unit = this.synchronized{ _map += Tuple2( hash, node ) }
    def apply( hash : Keccak256 ) : Node = this.synchronized{ _map(hash) }; 

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

    def EmptyHash : Keccak256 = EmbeddableEthStyleLowercaseTrie.EmptyHash

    private[this] def hash( node : Node ) : Keccak256 = {
      import java.io._;
      import com.mchange.sc.v2.lang.borrow;

      def b( byte : Byte ) : Byte = byte; // force coercion of Int to Byte
      def subkeyToBytes( subkey : Subkey ) : Seq[Byte] = subkey.map( c=> ((c & 0xFF).toByte) );
      def nseToBytes( nse : NodeSource.Embedded[Char,String,Keccak256] ) : Seq[Byte] = nodeToBytes( nse.node );
      def nshToBytes( nsh : NodeSource.Hash[Char,String,Keccak256] ) : Seq[Byte] = nsh.hash.bytes;
      def strToBytes( str : String ) : Seq[Byte] = str.getBytes("UTF-8").toSeq;
      def osToBytes( mbStr : Option[String] ) : Seq[Byte] = mbStr.fold( Seq( b(0) ) )( str => (b(0) +: str.getBytes("UTF-8")).toSeq :+ b(0) );
      def nsToBytes( ns  : NodeSource ) : Seq[Byte] = (ns : @unchecked) match {
        case nse : EmbeddableEthStylePMTrie.NodeSource.Embedded[Char,String,Keccak256] => nseToBytes( nse );
        case nsh : EmbeddableEthStylePMTrie.NodeSource.Hash[Char,String,Keccak256] => nshToBytes( nsh );
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
      Keccak256.hash( nodeToBytes( node ) )
    }
  }
}

class EmbeddableEthStyleLowercaseTrie( 
  mdb : EmbeddableEthStyleLowercaseTrie.MapDatabase = new EmbeddableEthStyleLowercaseTrie.MapDatabase, 
  r : Keccak256 = EmbeddableEthStyleLowercaseTrie.EmptyHash 
) extends {
  val earlyInit = EarlyInit( EmbeddableEthStyleLowercaseTrie.alphabet, mdb, r );
} with EmbeddableEthStylePMTrie[Char,String,Keccak256,EmbeddableEthStyleLowercaseTrie] {
  import EmbeddableEthStyleLowercaseTrie._;

  def instantiateSuccessor( newRootHash : Keccak256 ) : EmbeddableEthStyleLowercaseTrie = new EmbeddableEthStyleLowercaseTrie( mdb, newRootHash );
}

