package com.mchange.sc.v1.consuela;

import ethereum.encoding._;
import RLPSerializing.asEncodable; // implicit conversion

import com.mchange.sc.v1.consuela.hash.Hash;

import scala.collection._;

import scala.util.Try;

package object ethereum {
  class EthereumException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class UnexpectedSignatureFormatException( message : String, t : Throwable = null ) extends EthereumException( message, t );

  type EthHash    = Hash.SHA3_256;
  val  EthHash    = Hash.SHA3_256;
  val  EthHashLen = Hash.SHA3_256.HashLength;

  val EmptyByteSeqHash = EthHash.hash( encoding.RLP.Encoded.EmptyByteSeq )

  implicit object EthHashRLPSerializing extends RLPSerializing.ByteArrayValue[EthHash]( EthHash.withBytes );

  implicit object EthAddressRLPSerializing extends RLPSerializing.ByteArrayValue[EthAddress]( EthAddress.apply );

  implicit object EthTransactionRLPSerializing extends RLPSerializing[EthTransaction] {
    import EthTransaction._;

    override def toRLPEncodable( txn : EthTransaction ): RLP.Encodable = {
      import RLP.Encodable.{UnsignedBigInt => UBI, ByteSeq => BS, UnsignedInt => UI};

      def baseEncodables( unsigned : Unsigned ) : Vector[RLP.Encodable] = {
        val (rlpMbTo, payload) = unsigned match {
          case msg : Unsigned.Message          => (msg.to.bytes, msg.data);
          case cc  : Unsigned.ContractCreation => (Nil, cc.init);
        }
        Vector( UBI( unsigned.nonce ), UBI( unsigned.gasPrice ), UBI( unsigned.gasLimit ), BS( rlpMbTo ), UBI( unsigned.value ), BS( payload ) );
      }
      def sigEncodables( signed : Signed ) : Vector[RLP.Encodable] = Vector( UI( signed.v ), UBI( signed.r ), UBI( signed.s ) ) 

      txn match {
        case unsigned : Unsigned => RLP.Encodable.Seq( baseEncodables( unsigned ) );
        case signed   : Signed   => RLP.Encodable.Seq( baseEncodables( signed.base ) ++ sigEncodables( signed ) );
        case other               => throw new AssertionError( s"Huh? Saw an EthTransaction that is marked neither Signed nor Unsigned: ${other}" );
      }
    }
    override def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[EthTransaction] = {
      import RLP.Encodable.{ByteSeq => BS};

      def decodeMbToBytes( mbToBytes : Seq[Byte] ) : Failable[Option[EthAddress]] = {
        Try( if (mbToBytes == Nil) None else Some( EthAddress( mbToBytes ) ) ).toFailable
      }

      val RLP.Encodable.Seq.of( BS( nonceBytes ), BS( gasPriceBytes ), BS( gasLimitBytes ), BS( mbToBytes ), BS( valueBytes ), BS( payloadBytes ), rest @ _* ) = encodable;

      val base = for {
        nonce    <- RLP.decodeComplete[BigInt]( nonceBytes );
        gasPrice <- RLP.decodeComplete[BigInt]( gasPriceBytes );
        gasLimit <- RLP.decodeComplete[BigInt]( gasLimitBytes );
        mbTo     <- decodeMbToBytes( mbToBytes );
        value    <- RLP.decodeComplete[BigInt]( valueBytes );
        payload  <- RLP.decodeComplete[immutable.Seq[Byte]]( payloadBytes )
      } yield {
        mbTo.fold( new Unsigned.ContractCreation( nonce, gasPrice, gasLimit, value, payload.toIndexedSeq ) : Unsigned ){ addr =>
          new Unsigned.Message( nonce, gasPrice, gasLimit, addr, value, payload.toIndexedSeq )
        }
      }
      if ( rest.isEmpty ) {
        base
      } else {
        val Seq( BS( vBytes ), BS( rBytes ), BS( sBytes ) ) = rest;
        for {
          b        <- base;
          v        <- RLP.decodeComplete[Int]( vBytes );
          r        <- RLP.decodeComplete[BigInt]( rBytes );
          s        <- RLP.decodeComplete[BigInt]( sBytes );
          sig      <- Try( EthSignature( v.toByte, r, s ) ).toFailable
        } yield {
          Signed( b, sig )
        }
      }
    }
  }

  implicit object WorldStateAccountRLPSerializing extends RLPSerializing[WorldState.Account] {
    def toRLPEncodable( account : WorldState.Account ) : RLP.Encodable = {
      val codeHash = {
        account match {
          case contract : WorldState.Account.Contract => contract.codeHash;
          case agent    : WorldState.Account.Agent    => trie.EmptyTrieHash;
        }
      }

      import account._;
      RLP.Encodable.Seq.of( nonce, balance, storageRoot, codeHash );
    }
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[WorldState.Account] = {
      import RLP.Encodable.{ByteSeq => BS}
      val RLP.Encodable.Seq.of( BS( nonceBytes ), BS( balanceBytes ), BS( storageRootBytes ), BS( codeHashBytes ) ) = encodable;
      for {
        nonce       <- RLP.decodeComplete[BigInt]( nonceBytes );
        balance     <- RLP.decodeComplete[BigInt]( balanceBytes );
        storageRoot <- RLP.decodeComplete[EthHash]( storageRootBytes );
        codeHash    <- RLP.decodeComplete[EthHash]( codeHashBytes )
      } yield {
        codeHash match {
          case trie.EmptyTrieHash => WorldState.Account.Agent( nonce, balance, storageRoot );
          case _                  => WorldState.Account.Contract( nonce, balance, storageRoot, codeHash );
        }
      }
    }
  }

  implicit object EthBlockHeaderRLPSerializing extends RLPSerializing[EthBlock.Header] {
    def toRLPEncodable( header : EthBlock.Header ) : RLP.Encodable = {
      import header._
      RLP.Encodable.Seq.of(
        parentHash,
        ommersHash,
        coinbase,
        stateRoot,
        transactionRoot,
        receiptsRoot,
        logsBloom,
        difficulty,
        number,
        gasLimit,
        gasUsed,
        timestamp,
        extraData,
        mixHash,
        nonce
      )
    }
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[EthBlock.Header] = {
      import RLP.Encodable.{ByteSeq => BS}
      val RLP.Encodable.Seq.of(
        BS( parentHashBytes ),
        BS( ommersHashBytes ),
        BS( coinbaseBytes ),
        BS( stateRootBytes ),
        BS( transactionRootBytes ),
        BS( receiptsRootBytes ),
        BS( logsBloomBytes ),
        BS( difficultyBytes ),
        BS( numberBytes ),
        BS( gasLimitBytes ),
        BS( gasUsedBytes ),
        BS( timestampBytes ),
        BS( extraDataBytes ),
        BS( mixHashBytes ),
        BS( nonceBytes )
      ) = encodable;
      for {
        parentHash      <- RLP.decodeComplete[EthHash]( parentHashBytes );
        ommersHash      <- RLP.decodeComplete[EthHash]( ommersHashBytes );
        coinbase        <- RLP.decodeComplete[EthAddress]( coinbaseBytes );
        stateRoot       <- RLP.decodeComplete[EthHash]( stateRootBytes );
        transactionRoot <- RLP.decodeComplete[EthHash]( transactionRootBytes )
        receiptsRoot    <- RLP.decodeComplete[EthHash]( receiptsRootBytes );
        logsBloom       <- RLP.decodeComplete[BigInt]( logsBloomBytes );
        difficulty      <- RLP.decodeComplete[BigInt]( difficultyBytes );
        number          <- RLP.decodeComplete[BigInt]( numberBytes );
        gasLimit        <- RLP.decodeComplete[BigInt]( gasLimitBytes );
        gasUsed         <- RLP.decodeComplete[BigInt]( gasUsedBytes );
        timestamp       <- RLP.decodeComplete[BigInt]( timestampBytes );
        extraData       <- RLP.decodeComplete[immutable.Seq[Byte]]( extraDataBytes );
        mixHash         <- RLP.decodeComplete[EthHash]( mixHashBytes );
        nonce           <- RLP.decodeComplete[immutable.Seq[Byte]]( nonceBytes )
      } yield {
        EthBlock.Header(
          parentHash,
          ommersHash,
          coinbase,
          stateRoot,
          transactionRoot,
          receiptsRoot,
          logsBloom,
          difficulty,
          number,
          gasLimit,
          gasUsed,
          timestamp,
          extraData,
          mixHash,
          nonce
        )
      }
    }
  }
}




