package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.trie._;
import com.mchange.sc.v1.consuela.ethereum.encoding.{RLP, RLPSerializing};

import scala.collection.Traversable;

import specification.Types.Unsigned256;

object WorldState {
  object Account {
    case class Contract( nonce : Unsigned256, balance : Unsigned256, storageRoot : EthHash, codeHash : EthHash ) extends Account {
      require( codeHash != EmptyTrieHash );
    }
    case class Agent( nonce : Unsigned256, balance : Unsigned256, storageRoot : EthHash ) extends Account {
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
class WorldState( private val trie : SimpleEthTrie ) {
  import WorldState.Account;

  def this( db : EthTrieDb, rootHash : EthHash ) = this( new SimpleEthTrie( db, rootHash ) );
  def this( db : EthTrieDb ) = this( new SimpleEthTrie( db, EmptyTrieHash ) )

  val RootHash = trie.RootHash;

  def apply( address : EthAddress ) : Option[WorldState.Account] = trie( address.toNibbles ).map( acctBytes => RLP.decodeComplete[Account]( acctBytes ).get )

  def including( address : EthAddress, account : Account ) : WorldState = new WorldState( trie.including( address.toNibbles, RLP.encode(account) ) );
  def excluding( address : EthAddress ) : WorldState = new WorldState( trie.excluding( address.toNibbles ) );

  def + ( pair : (EthAddress, Account) ) : WorldState = this.including( pair._1, pair._2 );
  def - ( address : EthAddress ) : WorldState = this.excluding( address );

  def ++ ( traversable : Traversable[(EthAddress, Account)]  ) : WorldState = traversable.foldLeft( this )( ( ws, pair ) => ws + pair )
  def -- ( traversable : Traversable[EthAddress] ) : WorldState = traversable.foldLeft( this )( ( ws, address ) => ws - address )
}
