package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

import scala.collection._;
import scala.collection.immutable.Vector;
import scala.collection.immutable.Seq;

import java.util.Arrays;
import scala.util.hashing.MurmurHash3;

import encoding.{RLP, RLPSerializing};
import specification.Types.{SignatureV, SignatureR, SignatureS, Unsigned => UnsignedBigInt};

object EthTransaction {
  object Unsigned {
    case class Message( nonce : UnsignedBigInt, gasPrice : UnsignedBigInt, gasLimit : UnsignedBigInt, to : EthAddress, value : UnsignedBigInt, data : immutable.Seq[Byte] ) 
        extends Unsigned with EthTransaction.Message {
      def sign( privateKey : EthPrivateKey ) : Signed.Message = Signed.Message( this, privateKey.sign( RLP.encode[EthTransaction]( this ).toArray ) );
    }
    case class ContractCreation( nonce : UnsignedBigInt, gasPrice : UnsignedBigInt, gasLimit : UnsignedBigInt, value : UnsignedBigInt, init : immutable.Seq[Byte] ) 
        extends Unsigned with EthTransaction.ContractCreation {
      def sign( privateKey : EthPrivateKey ) : Signed.ContractCreation = Signed.ContractCreation( this, privateKey.sign( RLP.encode[EthTransaction]( this ).toArray ) );
    }
  }
  sealed trait Unsigned extends EthTransaction {
    override def signed = false;

    def sign( privateKey : EthPrivateKey ) : Signed;
  }
  object Signed {
    def apply( base : Unsigned, sig : EthSignature ) : Signed = {
      base match {
        case msg : Unsigned.Message          => Message( msg, sig );
        case cc  : Unsigned.ContractCreation => ContractCreation( cc, sig );
      }
    }
    case class Message( base : Unsigned.Message, val signature : EthSignature ) extends Signed with EthTransaction.Message {
      def nonce    : UnsignedBigInt      = base.nonce;
      def gasPrice : UnsignedBigInt      = base.gasPrice;
      def gasLimit : UnsignedBigInt      = base.gasLimit;
      def to       : EthAddress          = base.to; 
      def value    : UnsignedBigInt      = base.value;
      def data     : immutable.Seq[Byte] = base.data;
    }
    case class ContractCreation( base : Unsigned.ContractCreation, val signature : EthSignature ) extends Signed  with EthTransaction.ContractCreation {
      def nonce    : UnsignedBigInt      = base.nonce;
      def gasPrice : UnsignedBigInt      = base.gasPrice;
      def gasLimit : UnsignedBigInt      = base.gasLimit;
      def value    : UnsignedBigInt      = base.value;
      def init     : immutable.Seq[Byte] = base.init;
    }
  }
  sealed trait Signed extends EthTransaction {
    val base      : Unsigned;
    val signature : EthSignature;

    lazy val signedHash = EthHash.hash(RLP.encode[EthTransaction](base));

    def v : SignatureV = signature.v;
    def r : SignatureR = signature.r;
    def s : SignatureS = signature.s;

    lazy val senderPublicKey : EthPublicKey = {
      def fail : EthPublicKey = throw new EthereumException(s"Could not recover public key for signature ${signature} with signed hash '${signedHash}'");
      crypto.secp256k1.recoverPublicKeyBytesV( v.widen, r.widen.bigInteger, s.widen.bigInteger, signedHash.toByteArray ).fold( fail )( pubKeyBytes => EthPublicKey( pubKeyBytes ) );
    }
    lazy val sender : EthAddress = senderPublicKey.toAddress;

    override def signed = true;
  }
  trait Message extends EthTransaction {
    def to   : EthAddress; // not optional once we know we are a message
    def data : immutable.Seq[Byte];

    def isMessage = true;
  }
  trait ContractCreation extends EthTransaction {
    def init : immutable.Seq[Byte];

    def isMessage = false;
  }
}
sealed trait EthTransaction {
  def nonce    : UnsignedBigInt;
  def gasPrice : UnsignedBigInt;
  def gasLimit : UnsignedBigInt;
  def value    : UnsignedBigInt;

  def signed   : Boolean;

  def isMessage          : Boolean;
  def isContractCreation : Boolean = !this.isMessage;
}


