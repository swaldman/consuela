package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.Implicits._
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.trie._;

import scala.collection.Traversable;

object WorldState {
  object Account {
    def encodeRLP( account : Account ) : Seq[Byte] = {
      import RLP._;
      val encodable = Encodable.Seq( 
        Seq(
          Encodable.UnsignedBigInt( account.nonce ),
          Encodable.UnsignedBigInt( account.balance ),
          Encodable.ByteSeq( account.storageRoot.bytes ),
          Encodable.ByteSeq( account.codeHash.bytes )
        )
      );
      encode( encodable )
    }
    def decodeRLP( bytes : Seq[Byte] ) : (Account, Seq[Byte]) = {
      import RLP._;
      import Encodable.{ByteSeq => BS}
      val (encodable, rest) = decode( bytes );
      val Encodable.Seq( Seq( BS( nonceBytes ), BS( balanceBytes ), BS( storageRootBytes ), BS( codeHashBytes ) ) ) = encodable;
      val account = Account( BigInt(1, nonceBytes.toArray), BigInt(1, balanceBytes.toArray), EthHash.withBytes( storageRootBytes ), EthHash.withBytes( codeHashBytes ) );
      ( account, rest )
    }
    def decodeCompleteRLP( bytes : Seq[Byte] ) : Account = {
      val ( account, rest ) = decodeRLP( bytes );
      if ( rest.length > 0 ) {
        throw new IllegalArgumentException(
          s"Account.decodeCompleteRLP(...) expects exactly the bytes of an Account; received bytes for ${account} with 0x${rest.hex} left over."
        )
      } else {
        account
      }
    }
  }
  case class Account( nonce : BigInt, balance : BigInt, storageRoot : EthHash, codeHash : EthHash ) {
    lazy val rlpBytes = Account.encodeRLP( this );
  }
}
class WorldState( private val trie : SimpleEthTrie ) {
  import WorldState.Account;

  def this( db : EthTrieDb, rootHash : EthHash ) = this( new SimpleEthTrie( db, rootHash ) );
  def this( db : EthTrieDb ) = this( new SimpleEthTrie( db, EthTrieDb.EmptyHash ) )

  val RootHash = trie.RootHash;

  def apply( address : EthAddress ) : Option[WorldState.Account] = trie( address.toNibbles ).map( Account.decodeCompleteRLP )

  def including( address : EthAddress, account : Account ) : WorldState = new WorldState( trie.including( address.toNibbles, account.rlpBytes ) );
  def excluding( address : EthAddress ) : WorldState = new WorldState( trie.excluding( address.toNibbles ) );

  def + ( pair : (EthAddress, Account) ) : WorldState = this.including( pair._1, pair._2 );
  def - ( address : EthAddress ) : WorldState = this.excluding( address );

  def ++ ( traversable : Traversable[(EthAddress, Account)]  ) : WorldState = traversable.foldLeft( this )( ( ws, pair ) => ws + pair )
  def -- ( traversable : Traversable[EthAddress] ) : WorldState = traversable.foldLeft( this )( ( ws, address ) => ws - address )
}
