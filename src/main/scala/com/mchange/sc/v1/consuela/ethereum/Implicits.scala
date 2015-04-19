package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializer};
import trie.EmptyTrieHash;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import scala.collection._;

import scala.reflect.ClassTag;

object Implicits {

  class ByteArrayValueSerializer[T <: ByteArrayValue]( factory : immutable.Seq[Byte] => T )( implicit evidence : ClassTag[T] ) extends RLPSerializer[T]()( evidence ) {
    def toRLPEncodable( t : T )                             : RLP.Encodable = RLP.Encodable.ByteSeq( t.bytes );
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[T] = {
      encodable match { 
        case RLP.Encodable.ByteSeq( bytes ) => Some( factory( bytes ) )
        case _                              => None
      }
    }
  }

  implicit object EthHashSerializer extends ByteArrayValueSerializer[EthHash]( EthHash.withBytes );
  implicit object EthAddressSerializer extends ByteArrayValueSerializer[EthAddress]( EthAddress.apply );

  implicit object AccountSerializer extends RLPSerializer[WorldState.Account] {
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
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Option[WorldState.Account] = {
      import RLP._;
      import Encodable.{ByteSeq => BS}
      val Encodable.Seq( Seq( BS( nonceBytes ), BS( balanceBytes ), BS( storageRootBytes ), BS( codeHashBytes ) ) ) = encodable;
      EthHash.withBytes( codeHashBytes ) match {
        case EmptyTrieHash => Some( WorldState.Account.Agent( BigInt(1, nonceBytes.toArray), BigInt(1, balanceBytes.toArray), EthHash.withBytes( storageRootBytes ) ) );
        case codeHash      => Some( WorldState.Account.Contract( BigInt(1, nonceBytes.toArray), BigInt(1, balanceBytes.toArray), EthHash.withBytes( storageRootBytes ), codeHash ) );
      }
    }
  }

  implicit class RLPOps[ T : RLPSerializer ]( rlpSerializable : T ) {
    def rlpEncodable : RLP.Encodable       = implicitly[RLPSerializer[T]].toRLPEncodable( rlpSerializable.asInstanceOf[T] );
    def rlpBytes     : immutable.Seq[Byte] = RLP.encode( this.rlpEncodable );
  }

  trait LazyRLPOps[T] {
    this : T =>

    val rlpOpsView : T => RLPOps[T];

    lazy val rlpEncodable : RLP.Encodable       = rlpOpsView( this ).rlpEncodable;
    lazy val rlpBytes     : immutable.Seq[Byte] = RLP.encode( this.rlpEncodable );
  }
}
