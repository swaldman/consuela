package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.trie._;
import com.mchange.sc.v1.consuela.ethereum.encoding.{RLP, RLPSerializing};

import scala.collection.Traversable;

import specification.Set.Unsigned256;

object WorldState {
  object Account {
    case class Contract( nonce : BigInt, balance : BigInt, storageRoot : EthHash, codeHash : EthHash ) extends Account {
      require( (nonce elem_: Unsigned256) && (balance elem_: Unsigned256) && codeHash != EmptyTrieHash );
    }
    case class Agent( nonce : BigInt, balance : BigInt, storageRoot : EthHash ) extends Account {
      require( (nonce elem_: Unsigned256) && (balance elem_: Unsigned256) );

      def codeHash = EmptyTrieHash;
    }

    implicit object AccountSerializer extends RLPSerializing[Account] {
      def toRLPEncodable( account : WorldState.Account ) : RLP.Encodable = {
        val codeHash = {
          account match {
            case contract : WorldState.Account.Contract => contract.codeHash;
            case agent    : WorldState.Account.Agent    => EmptyTrieHash;
          }
        }

        import RLP._;
        Encodable.Seq.of(
          Encodable.UnsignedBigInt( account.nonce ),
          Encodable.UnsignedBigInt( account.balance ),
          Encodable.ByteSeq( account.storageRoot.bytes ),
          Encodable.ByteSeq( codeHash.bytes )
        )
      }
      def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[Account] = {
        import RLP._;
        import Encodable.{ByteSeq => BS}
        val Encodable.Seq( Seq( BS( nonceBytes ), BS( balanceBytes ), BS( storageRootBytes ), BS( codeHashBytes ) ) ) = encodable;
        EthHash.withBytes( codeHashBytes ) match {
          case EmptyTrieHash => Some( WorldState.Account.Agent( BigInt(1, nonceBytes.toArray), BigInt(1, balanceBytes.toArray), EthHash.withBytes( storageRootBytes ) ) );
          case codeHash      => Some( WorldState.Account.Contract( BigInt(1, nonceBytes.toArray), BigInt(1, balanceBytes.toArray), EthHash.withBytes( storageRootBytes ), codeHash ) );
        }
      }
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

  def apply( address : EthAddress ) : Option[WorldState.Account] = trie( address.toNibbles ).flatMap( RLP.decodeComplete[Account] )

  def including( address : EthAddress, account : Account ) : WorldState = new WorldState( trie.including( address.toNibbles, RLP.encode(account) ) );
  def excluding( address : EthAddress ) : WorldState = new WorldState( trie.excluding( address.toNibbles ) );

  def + ( pair : (EthAddress, Account) ) : WorldState = this.including( pair._1, pair._2 );
  def - ( address : EthAddress ) : WorldState = this.excluding( address );

  def ++ ( traversable : Traversable[(EthAddress, Account)]  ) : WorldState = traversable.foldLeft( this )( ( ws, pair ) => ws + pair )
  def -- ( traversable : Traversable[EthAddress] ) : WorldState = traversable.foldLeft( this )( ( ws, address ) => ws - address )
}
