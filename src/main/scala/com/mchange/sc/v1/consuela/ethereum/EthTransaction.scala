package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.Implicits._;

import scala.collection.immutable.Vector;
import scala.collection.immutable.IndexedSeq;

import java.util.Arrays;
import scala.util.hashing.MurmurHash3;

import encoding.RLP;
import specification.Set.Unsigned256;

object EthTransaction {
  object Abstract {
    abstract class Message( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, val to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) 
        extends Abstract( nonce, gasPrice, gasLimit, Some(to), value, dataBytes ) with EthTransaction.Message {
      def data : IndexedSeq[Byte] = payload;
    }
    abstract class ContractCreation( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) 
        extends Abstract( nonce, gasPrice, gasLimit, None, value, initBytes )  with EthTransaction.ContractCreation {
      def init : IndexedSeq[Byte] = payload;
    }
  }
  abstract class Abstract( 
    val nonce : BigInt,
    val gasPrice : BigInt, 
    val gasLimit : BigInt, 
    val mbTo : Option[EthAddress], 
    val value : BigInt, 
    private[EthTransaction] val payloadBytes : Array[Byte] 
  ) extends EthTransaction {
    require( (nonce elem_!: Unsigned256) && (gasPrice elem_!: Unsigned256) && (gasLimit elem_!: Unsigned256) );

    protected lazy val payload : IndexedSeq[Byte] = Vector( payloadBytes : _* );

    protected final def sameBase( other : Abstract ) = {
      this.nonce == other.nonce &&
      this.gasPrice == other.gasPrice &&
      this.gasLimit == other.gasLimit &&
      this.mbTo == other.mbTo &&
      Arrays.equals( this.payloadBytes, other.payloadBytes )
    }
    protected lazy val baseHash = nonce.## ^ gasPrice.## ^ gasLimit.## ^ mbTo.## ^ MurmurHash3.bytesHash( payloadBytes )

    protected def encodableNonce    = RLP.Encodable.UnsignedBigInt( nonce );
    protected def encodableGasPrice = RLP.Encodable.UnsignedBigInt( gasPrice );
    protected def encodableMbTo     = mbTo.fold( RLP.Encodable.EmptyByteSeq )( address => RLP.Encodable.ByteSeq( address.bytes ) );
    protected def encodableValue    = RLP.Encodable.UnsignedBigInt( value );
    protected def encodablePayload  = RLP.Encodable.ByteSeq( payloadBytes );

    protected def baseRlpElements : Seq[RLP.Encodable] = Seq( encodableNonce, encodableGasPrice, encodableMbTo, encodableValue, encodablePayload );
    protected def baseRlp         : Seq[Byte]          = RLP.Encodable.encode( RLP.Encodable.Seq( baseRlpElements ) );
  }


  object Unsigned {
    object Message { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) = {
        new Message( nonce, gasPrice, gasLimit, to, value, dataBytes.clone() )
      }
    }
    final class Message private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) 
        extends Abstract.Message( nonce, gasPrice, gasLimit, to, value, dataBytes ) with Unsigned {
      def sign( privateKey : EthPrivateKey ) : Signed.Message = Signed.Message( this, privateKey.sign( this.baseRlp.toArray ) );
    }

    object ContractCreation { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) = new ContractCreation( nonce, gasPrice, gasLimit, value, initBytes.clone() )
    }
    final class ContractCreation private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) 
        extends Abstract.ContractCreation( nonce, gasPrice, gasLimit, value, initBytes ) with Unsigned {
      def sign( privateKey : EthPrivateKey ) : Signed.ContractCreation = Signed.ContractCreation( this, privateKey.sign( this.baseRlp.toArray ) );
    }
  }
  sealed trait Unsigned extends Abstract {
    override def signed = false;

    def sign( privateKey : EthPrivateKey ) : Signed;

    override def equals( a : Any ) : Boolean = {
      a match {
        case other : Unsigned => this.sameBase( other );
        case _                => false;
      }
    }
    override def hashCode() : Int = baseHash;
  }


  object Signed {
    object Message { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte], signature : EthSignature ) = {
        new Message( nonce, gasPrice, gasLimit, to, value, dataBytes.clone(), signature )
      }
      private[EthTransaction] def apply( unsigned : Unsigned.Message, signature : EthSignature ) = { // no need for defensive array clone(), both signed and unsigned are immutable
        new Message( unsigned.nonce, unsigned.gasPrice, unsigned.gasLimit, unsigned.to, unsigned.value, unsigned.payloadBytes, signature )
      }
    }
    final class Message private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte], val signature : EthSignature ) 
        extends Abstract.Message( nonce, gasPrice, gasLimit, to, value, dataBytes ) with Signed;

    object ContractCreation { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte], signature : EthSignature ) = {
        new ContractCreation( nonce, gasPrice, gasLimit, value, initBytes.clone(), signature )
      }
      private[EthTransaction] def apply( unsigned : Unsigned.ContractCreation, signature : EthSignature ) = { // no need for defensive array clone(), both signed and unsigned are immutable
        new ContractCreation( unsigned.nonce, unsigned.gasPrice, unsigned.gasLimit, unsigned.value, unsigned.payloadBytes, signature )
      }
    }
    final class ContractCreation private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte], val signature : EthSignature ) 
        extends Abstract.ContractCreation( nonce, gasPrice, gasLimit, value, initBytes ) with Signed;
  }
  sealed trait Signed extends Abstract {
    val signature   : EthSignature;

    lazy val signedHash = EthHash.hash(baseRlp.toArray);

    def v : Byte   = signature.v;
    def r : BigInt = signature.r;
    def s : BigInt = signature.s;

    lazy val senderPublicKey : EthPublicKey = {
      def fail : EthPublicKey = throw new EthereumException(s"Could not recover public key for signature ${signature} with signed hash '${signedHash}'");
      crypto.secp256k1.recoverPublicKeyBytesV( v, r.bigInteger, s.bigInteger, signedHash.toByteArray ).fold( fail )( pubKeyBytes => EthPublicKey( pubKeyBytes ) );
    }
    lazy val sender : EthAddress = senderPublicKey.toAddress;

    override def signed = true;

    override def equals( a : Any ) : Boolean = {
      a match {
        case other : Signed => this.sameBase( other ) && this.signature == other.signature;
        case _              => false;
      }
    }
    override def hashCode() : Int = baseHash ^ signature.##;

    private def sigRlpElements : Seq[RLP.Encodable] = Seq( RLP.Encodable.UnsignedInt( v.toInt ), RLP.Encodable.UnsignedBigInt( r ), RLP.Encodable.UnsignedBigInt( s ) );
    lazy val rlpBytes : Seq[Byte] = RLP.Encodable.encode( RLP.Encodable.Seq( baseRlpElements ++ sigRlpElements ) );
  }
  trait Message extends EthTransaction {
    def to   : EthAddress; // not optional once we know we are a message
    def data : IndexedSeq[Byte];

    def isMessage = true;
  }
  trait ContractCreation extends EthTransaction {
    def init : IndexedSeq[Byte];

    def isMessage = true;
  }
}
sealed trait EthTransaction {
  def nonce    : BigInt;
  def gasPrice : BigInt;
  def gasLimit : BigInt;
  def mbTo     : Option[EthAddress];
  def value    : BigInt;

  def signed   : Boolean;

  def isMessage          : Boolean;
  def isContractCreation : Boolean = !this.isMessage;
}


