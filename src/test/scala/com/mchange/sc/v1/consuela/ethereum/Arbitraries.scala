package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.ethereum._;
import specification.Types._;
import specification.Arbitraries._;

import com.mchange.sc.v1.consuela.bloom.BitSetBloom;

import scala.collection._;

import org.scalacheck._;
import Arbitrary.arbitrary;

object Arbitraries {
  def genByteArrayN( n : Int ) : Gen[Array[Byte]] = Gen.containerOfN[Array,Byte]( n, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) )

  implicit val ArbitraryEthAddress : Arbitrary[EthAddress] = Arbitrary( genByteArrayN( EthAddress.ByteLength ).map( EthAddress(_) ) )
  implicit val ArbitraryEthHash    : Arbitrary[EthHash]    = Arbitrary( genByteArrayN( EthHashLen ).map( EthHash.withBytes( _ ) ) )

  val GenTransactionUnsignedMessage : Gen[EthTransaction.Unsigned.Message] = {
    for {
      nonce       <- arbitrary[Unsigned256];
      gasPrice    <- arbitrary[Unsigned256];
      gasLimit    <- arbitrary[Unsigned256];
      to          <- arbitrary[EthAddress]
      value       <- arbitrary[Unsigned256];
      data        <- arbitrary[immutable.Seq[Byte]]
    } yield {
      EthTransaction.Unsigned.Message( nonce, gasPrice, gasLimit, to, value, data )
    }
  }
  val GenTransactionUnsignedContractCreation : Gen[EthTransaction.Unsigned.ContractCreation] = {
    for {
      nonce       <- arbitrary[Unsigned256];
      gasPrice    <- arbitrary[Unsigned256];
      gasLimit    <- arbitrary[Unsigned256];
      value       <- arbitrary[Unsigned256];
      init        <- arbitrary[immutable.Seq[Byte]]
    } yield {
      EthTransaction.Unsigned.ContractCreation( nonce, gasPrice, gasLimit, value, init )
    }
  }
  val GenTransactionUnsigned : Gen[EthTransaction.Unsigned] = Gen.oneOf( GenTransactionUnsignedMessage, GenTransactionUnsignedContractCreation );

  implicit val ArbitraryTransactionUnsigned = Arbitrary( GenTransactionUnsigned );

  val GenEthSignature : Gen[EthSignature] = {
    for {
      v <- arbitrary[SignatureV];
      r <- arbitrary[SignatureR];
      s <- arbitrary[SignatureS]
    } yield {
      EthSignature( v, r, s )
    }
  }

  implicit val ArbitraryEthSignature = Arbitrary( GenEthSignature );

  val GenTransactionSigned : Gen[EthTransaction.Signed] = {
    for {
      unsigned <- arbitrary[EthTransaction.Unsigned];
      sig      <- arbitrary[EthSignature]
    } yield {
      EthTransaction.Signed( unsigned, sig )
    }
  }
  val GenTransaction : Gen[EthTransaction] = Gen.oneOf(GenTransactionUnsigned, GenTransactionSigned);

  implicit val ArbitraryTransaction : Arbitrary[EthTransaction] = Arbitrary( GenTransaction );

  val GenContractAccount = {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash];
      codeHash    <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Contract( nonce, balance, storageRoot, codeHash )
    }
  }
  val GenAgentAccount = {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Agent( nonce, balance, storageRoot )
    }
  }
  implicit val ArbitraryContractAccount : Arbitrary[EthWorldState.Account.Contract] = Arbitrary( GenContractAccount );
  implicit val ArbitraryAgentAccount    : Arbitrary[EthWorldState.Account.Agent]    = Arbitrary( GenAgentAccount );
  implicit val ArbitraryAccount         : Arbitrary[EthWorldState.Account]          = Arbitrary( Gen.oneOf( GenContractAccount, GenAgentAccount ) );

  val GenLogBloom = for ( seq <- arbitrary[ByteSeqExact256] ) yield BitSetBloom.fromBytes[EthLogEntry]( seq.widen )
  implicit val ArbitraryEthLogBloom : Arbitrary[EthLogBloom] = Arbitrary( GenLogBloom )

  val GenBlockHeader = {
    for {
      parentHash      <- arbitrary[EthHash];
      ommersHash      <- arbitrary[EthHash];
      coinbase        <- arbitrary[EthAddress];
      stateRoot       <- arbitrary[EthHash];
      transactionRoot <- arbitrary[EthHash];
      receiptsRoot    <- arbitrary[EthHash];
      logsBloom       <- arbitrary[EthLogBloom];
      difficulty      <- arbitrary[Unsigned256];
      number          <- arbitrary[Unsigned256];
      gasLimit        <- arbitrary[Unsigned256];
      gasUsed         <- arbitrary[Unsigned256];
      timestamp       <- arbitrary[Unsigned256];
      extraData       <- arbitrary[ByteSeqMax1024];
      mixHash         <- arbitrary[EthHash];
      nonce           <- arbitrary[ByteSeqExact8]
    } yield {
      EthBlock.Header( parentHash, ommersHash, coinbase, stateRoot, transactionRoot, receiptsRoot, logsBloom, difficulty, number, gasLimit, gasUsed, timestamp, extraData, mixHash, nonce )
    }
  }
  implicit val ArbitraryBlockHeader : Arbitrary[EthBlock.Header] = Arbitrary( GenBlockHeader );

  val GenBlock : Gen[EthBlock] = {
    for {
      header       <- arbitrary[EthBlock.Header];
      transactions <- arbitrary[immutable.Seq[EthTransaction]];
      ommers       <- arbitrary[immutable.Seq[EthBlock.Header]]
    } yield {
      EthBlock( header, transactions, ommers )
    }
  }
  implicit val ArbitraryBlock = Arbitrary( GenBlock );
}
