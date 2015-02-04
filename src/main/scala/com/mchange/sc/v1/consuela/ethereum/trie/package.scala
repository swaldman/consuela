package com.mchange.sc.v1.consuela.ethereum;

import scala.collection.mutable.ArrayBuffer;

import com.mchange.sc.v1.consuela.hash.Hash;
import com.mchange.sc.v1.consuela.trie.BasicPMTrie;

package object trie {
  type Nibble = Int;

  val  EthHash = Hash.SHA3_256;
  type EthHash = Hash.SHA3_256;

  type Node = BasicPMTrie.Node[Nibble,Seq[Byte],EthHash]
  type Branch = BasicPMTrie.Branch[Nibble,Seq[Byte],EthHash]
  type Extension = BasicPMTrie.Extension[Nibble,Seq[Byte],EthHash]
  val  Empty = BasicPMTrie.Empty;

  def nibbles( bytes : Seq[Byte] ) = lsbNibbles( bytes.map( _ & 0xFF ) )

  /**
   *  Least significant byte nibbles
   */ 
  def lsbNibbles( bytes : Seq[Int] ) : Seq[Int] = {
    val out = new Array[Int]( bytes.length * 2 );
    var idx = 0;
    bytes.foreach {  byte => 
      out(idx) = (byte & 0xF0) >>> 4;
      out(idx+1) = byte & 0x0F;
      idx += 2
    }
    out.toSeq
  }
}
