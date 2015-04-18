package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.Implicits._;
import com.mchange.sc.v1.consuela.ethereum.trie._;
import com.mchange.sc.v1.consuela.ethereum.encoding.{RLP, RLPSerializer};

import scala.collection.Traversable;

import specification.Set.Unsigned256;

object WorldState {
  object Account extends RLPSerializer.Wrapper[Account] {

    val serializer : RLPSerializer[Account] = implicitly[RLPSerializer[Account]]

    case class Contract( nonce : BigInt, balance : BigInt, storageRoot : EthHash, codeHash : EthHash ) extends Account {
      require( (nonce elem_: Unsigned256) && (balance elem_: Unsigned256) && codeHash != EmptyTrieHash );
    }
    case class Agent( nonce : BigInt, balance : BigInt, storageRoot : EthHash ) extends Account {
      require( (nonce elem_: Unsigned256) && (balance elem_: Unsigned256) );

      def codeHash = EmptyTrieHash;
    }
  }
  sealed trait Account {
    def nonce       : BigInt;
    def balance     : BigInt;
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

  def apply( address : EthAddress ) : Option[WorldState.Account] = trie( address.toNibbles ).flatMap( Account.decodeCompleteRLP )

  def including( address : EthAddress, account : Account ) : WorldState = new WorldState( trie.including( address.toNibbles, account.rlpBytes ) );
  def excluding( address : EthAddress ) : WorldState = new WorldState( trie.excluding( address.toNibbles ) );

  def + ( pair : (EthAddress, Account) ) : WorldState = this.including( pair._1, pair._2 );
  def - ( address : EthAddress ) : WorldState = this.excluding( address );

  def ++ ( traversable : Traversable[(EthAddress, Account)]  ) : WorldState = traversable.foldLeft( this )( ( ws, pair ) => ws + pair )
  def -- ( traversable : Traversable[EthAddress] ) : WorldState = traversable.foldLeft( this )( ( ws, address ) => ws - address )
}
