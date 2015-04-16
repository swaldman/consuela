package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.Implicits._;

import scala.collection.immutable.Vector;
import scala.collection.immutable.IndexedSeq;

import java.util.Arrays;
import scala.util.hashing.MurmurHash3;

import specification.Set.Unsigned256;

object EthTransaction {
  object Abstract {
    abstract class Message( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) 
        extends Abstract( nonce, gasPrice, gasLimit, Some(to), value, dataBytes ) {
      def data : IndexedSeq[Byte] = payload;
    }
    abstract class ContractCreation( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) 
        extends Abstract( nonce, gasPrice, gasLimit, None, value, initBytes ) {
      def init : IndexedSeq[Byte] = payload;
    }
  }
  abstract class Abstract( 
    val nonce : BigInt,
    val gasPrice : BigInt, 
    val gasLimit : BigInt, 
    val to : Option[EthAddress], 
    val value : BigInt, 
    private[EthTransaction] val payloadBytes : Array[Byte] 
  ) extends EthTransaction {
    require( (nonce elem_!: Unsigned256) && (gasPrice elem_!: Unsigned256) && (gasLimit elem_!: Unsigned256) );

    protected lazy val payload : IndexedSeq[Byte] = Vector( payloadBytes : _* );

    protected final def sameBase( other : Abstract ) = {
      this.nonce == other.nonce &&
      this.gasPrice == other.gasPrice &&
      this.gasLimit == other.gasLimit &&
      this.to == other.to &&
      Arrays.equals( this.payloadBytes, other.payloadBytes )
    }
    protected lazy val baseHash = nonce.## ^ gasPrice.## ^ gasLimit.## ^ to.## ^ MurmurHash3.bytesHash( payloadBytes )

    protected def encodableNonce    = RLP.Encodable.UnsignedBigInt( nonce );
    protected def encodableGasPrice = RLP.Encodable.UnsignedBigInt( gasPrice );
    protected def encodableTo       = to.fold( RLP.Encodable.EmptyByteSeq )( address => RLP.Encodable.ByteSeq( address.bytes ) );
    protected def encodableValue    = RLP.Encodable.UnsignedBigInt( value );
    protected def encodablePayload  = RLP.Encodable.ByteSeq( payloadBytes );

    protected def baseRLP : Seq[Byte] = RLP.encode( RLP.Encodable.Seq.of( encodableNonce, encodableGasPrice, encodableTo, encodableValue, encodablePayload ) );
  }
  object Unsigned {
    object Message { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) = {
        new Message( nonce, gasPrice, gasLimit, to, value, dataBytes.clone() )
      }
    }
    final class Message private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, to : EthAddress, value : BigInt, dataBytes : Array[Byte] ) 
        extends Abstract.Message( nonce, gasPrice, gasLimit, to, value, dataBytes ) with Unsigned {
      def sign( privateKey : EthPrivateKey )( implicit provider : jce.Provider ) : Signed.Message = Signed.Message( this, privateKey.sign( this.baseRLP.toArray )( provider ) );
    }

    object ContractCreation { // note the defensive array clone()!
      def apply( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) = new ContractCreation( nonce, gasPrice, gasLimit, value, initBytes.clone() )
    }
    final class ContractCreation private ( nonce : BigInt, gasPrice : BigInt, gasLimit : BigInt, value : BigInt, initBytes : Array[Byte] ) 
        extends Abstract.ContractCreation( nonce, gasPrice, gasLimit, value, initBytes ) with Unsigned {
      def sign( privateKey : EthPrivateKey )( implicit provider : jce.Provider ) : Signed.ContractCreation = Signed.ContractCreation( this, privateKey.sign( this.baseRLP.toArray )( provider ) );
    }
  }
  sealed trait Unsigned extends Abstract {
    override def signed = false;

    def sign( privateKey : EthPrivateKey )( implicit provider : jce.Provider ) : Signed;

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
        new Message( unsigned.nonce, unsigned.gasPrice, unsigned.gasLimit, unsigned.to.get, unsigned.value, unsigned.payloadBytes, signature )
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

    lazy val signedHash = EthHash.hash(baseRLP.toArray);

    def v : Byte   = signature.v;
    def r : BigInt = signature.r;
    def s : BigInt = signature.s;

    lazy val senderPublicKey : EthPublicKey = {
      //XXX TODO: Do the best that can be done to make this more sensitive to provider
      jce.Provider.warnForbidUnconfiguredUseOfBouncyCastle( this )

      def fail : EthPublicKey = throw new EthereumException(s"Could not recover public key for signature ${signature} with signed hash '${signedHash}'");
      crypto.secp256k1.BouncyCastlePublicKeyComputer.recoverPublicKeyBytesV( v, r.bigInteger, s.bigInteger, signedHash.toByteArray ).fold( fail )( pubKeyBytes => EthPublicKey( pubKeyBytes ) );
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
  }
  trait Message extends EthTransaction {
    def data : IndexedSeq[Byte];
  }
  trait ContractCreation extends EthTransaction {
    def init : IndexedSeq[Byte];
  }
}
sealed trait EthTransaction {
  def nonce    : BigInt;
  def gasPrice : BigInt;
  def gasLimit : BigInt;
  def to       : Option[EthAddress];
  def value    : BigInt;

  def signed   : Boolean;
}


