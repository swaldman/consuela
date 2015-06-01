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

package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.trie._;
import com.mchange.sc.v1.consuela.ethereum.encoding.RLP;

import scala.collection.Traversable;

import specification.Types.Unsigned256;

object EthWorldState {
  final object Account {
    def apply( nonce : Unsigned256, balance : Unsigned256, storageRoot : EthHash, codeHash : EthHash ) : EthWorldState.Account = {
      codeHash match {
        case EmptyTrieHash => Agent( nonce, balance, storageRoot );
        case nonempty      => Contract( nonce, balance, storageRoot, nonempty );
      }
    }
    final case class Contract( nonce : Unsigned256, balance : Unsigned256, storageRoot : EthHash, codeHash : EthHash ) extends Account {
      require( codeHash != EmptyTrieHash );
    }
    final case class Agent( nonce : Unsigned256, balance : Unsigned256, storageRoot : EthHash ) extends Account {
      def codeHash = EmptyTrieHash;
    }
  }
  sealed trait Account {
    def nonce       : Unsigned256;
    def balance     : Unsigned256;
    def storageRoot : EthHash;
    def codeHash    : EthHash;

    def isAgent    : Boolean = codeHash == EmptyTrieHash;
    def isContract : Boolean = !this.isAgent;
  }
}
final class EthWorldState( private val trie : SimpleEthTrie ) {
  import EthWorldState.Account;

  def this( db : EthTrieDb, rootHash : EthHash ) = this( new SimpleEthTrie( db, rootHash ) );
  def this( db : EthTrieDb ) = this( new SimpleEthTrie( db, EmptyTrieHash ) )

  val RootHash = trie.RootHash;

  def apply( address : EthAddress ) : Option[EthWorldState.Account] = trie( address.toNibbles ).map( acctBytes => RLP.decodeComplete[Account]( acctBytes ).get )

  def including( address : EthAddress, account : Account ) : EthWorldState = new EthWorldState( trie.including( address.toNibbles, RLP.encode(account) ) );
  def excluding( address : EthAddress ) : EthWorldState = new EthWorldState( trie.excluding( address.toNibbles ) );

  def + ( pair : (EthAddress, Account) ) : EthWorldState = this.including( pair._1, pair._2 );
  def - ( address : EthAddress ) : EthWorldState = this.excluding( address );

  def ++ ( traversable : Traversable[(EthAddress, Account)]  ) : EthWorldState = traversable.foldLeft( this )( ( ws, pair ) => ws + pair )
  def -- ( traversable : Traversable[EthAddress] ) : EthWorldState = traversable.foldLeft( this )( ( ws, address ) => ws - address )
}
