package com.mchange.sc.v1.consuela;

import ethereum.encoding._;
import RLPSerializing.asEncodable;  // implicit conversion
import RLPSerializing.asEncodables; // not implicit 

import com.mchange.sc.v1.consuela.hash.Hash;

import scala.collection._;

import scala.util.Try;

import com.mchange.sc.v1.log._;
import MLevel._;

package object ethereum {
  implicit lazy val logger = MLogger( this );

  class EthereumException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class UnexpectedSignatureFormatException( message : String, t : Throwable = null ) extends EthereumException( message, t );

  type EthHash    = Hash.SHA3_256;
  val  EthHash    = Hash.SHA3_256;
  val  EthHashLen = Hash.SHA3_256.HashLength;

  val EmptyByteSeqHash = EthHash.hash( encoding.RLP.Encoded.EmptyByteSeq );

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
      def fromMbToEncodable( mbToEncodable : RLP.Encodable.Basic ) : Failable[Option[EthAddress]] = {
        mbToEncodable match {
          case RLP.Encodable.ByteSeq( mbToBytes ) => Try( if (mbToBytes == Nil) None else Some( EthAddress( mbToBytes ) ) ).toFailable;
          case whatever                           => failNotLeaf( whatever );
        }
      }
      encodable match {
        case RLP.Encodable.Seq.of( nonceE, gasPriceE, gasLimitE, mbToE, valueE, payloadE, rest @ _* ) => {
          val base = for {
            nonce    <- RLP.fromEncodable[BigInt]( nonceE.simplify );
            gasPrice <- RLP.fromEncodable[BigInt]( gasPriceE.simplify );
            gasLimit <- RLP.fromEncodable[BigInt]( gasLimitE.simplify );
            mbTo     <- fromMbToEncodable( mbToE.simplify );
            value    <- RLP.fromEncodable[BigInt]( valueE.simplify );
            payload  <- RLP.fromEncodable[immutable.Seq[Byte]]( payloadE.simplify )
          } yield {
            mbTo.fold( new Unsigned.ContractCreation( nonce, gasPrice, gasLimit, value, payload.toIndexedSeq ) : Unsigned ){ addr =>
              new Unsigned.Message( nonce, gasPrice, gasLimit, addr, value, payload.toIndexedSeq )
            }
          }
          if ( rest.isEmpty ) {
            base
          } else {
            rest match {
              case Seq( vE, rE, sE ) => {
                for {
                  b        <- base;
                  v        <- RLP.fromEncodable[Int]( vE.simplify );
                  r        <- RLP.fromEncodable[BigInt]( rE.simplify );
                  s        <- RLP.fromEncodable[BigInt]( sE.simplify );
                  sig      <- Try( EthSignature( v.toByte, r, s ) ).toFailable
                } yield {
                  Signed( b, sig )
                }
              }
              case uhhuh => fail( s"After base transaction elements, expected a three part signature, instead found '${uhhuh}'." );
            }
          }
        }
        case whatever => fail( s"Expected a sequence of at least 6 ByteSeqs. Instead found '${whatever}'" );
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
      encodable match {
        case RLP.Encodable.Seq.of( nonceE , balanceE, storageRootE, codeHashE ) => {
          for {
            nonce       <- RLP.fromEncodable[BigInt]( nonceE.simplify );
            balance     <- RLP.fromEncodable[BigInt]( balanceE.simplify );
            storageRoot <- RLP.fromEncodable[EthHash]( storageRootE.simplify );
            codeHash    <- RLP.fromEncodable[EthHash]( codeHashE.simplify )
          } yield {
            codeHash match {
              case trie.EmptyTrieHash => WorldState.Account.Agent( nonce, balance, storageRoot );
              case _                  => WorldState.Account.Contract( nonce, balance, storageRoot, codeHash );
            }
          }
        }
        case other => fail( s"Expected ( nonceE , balanceE, storageRootE, codeHashE ), found ${other}" );
      }
    }
  }

  implicit object EthBlockHeaderRLPSerializing extends RLPSerializing[EthBlock.Header] {
    def toRLPEncodable( header : EthBlock.Header ) : RLP.Encodable = {
      import header._
      RLP.Encodable.Seq.of( 
        parentHash, ommersHash, coinbase, stateRoot, transactionRoot, receiptsRoot, logsBloom,
        difficulty, number, gasLimit, gasUsed, timestamp, extraData, mixHash, nonce
      )
    }
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[EthBlock.Header] = {
      encodable match {
        case RLP.Encodable.Seq.of(
          parentHashE, ommersHashE, coinbaseE, stateRootE, transactionRootE, receiptsRootE, logsBloomE,
          difficultyE, numberE, gasLimitE, gasUsedE, timestampE, extraDataE, mixHashE, nonceE
        ) => {
          for {
            parentHash      <- RLP.fromEncodable[EthHash]( parentHashE.simplify );
            ommersHash      <- RLP.fromEncodable[EthHash]( ommersHashE.simplify );
            coinbase        <- RLP.fromEncodable[EthAddress]( coinbaseE.simplify );
            stateRoot       <- RLP.fromEncodable[EthHash]( stateRootE.simplify );
            transactionRoot <- RLP.fromEncodable[EthHash]( transactionRootE.simplify )
            receiptsRoot    <- RLP.fromEncodable[EthHash]( receiptsRootE.simplify );
            logsBloom       <- RLP.fromEncodable[BigInt]( logsBloomE.simplify );
            difficulty      <- RLP.fromEncodable[BigInt]( difficultyE.simplify );
            number          <- RLP.fromEncodable[BigInt]( numberE.simplify );
            gasLimit        <- RLP.fromEncodable[BigInt]( gasLimitE.simplify );
            gasUsed         <- RLP.fromEncodable[BigInt]( gasUsedE.simplify );
            timestamp       <- RLP.fromEncodable[BigInt]( timestampE.simplify );
            extraData       <- RLP.fromEncodable[immutable.Seq[Byte]]( extraDataE.simplify );
            mixHash         <- RLP.fromEncodable[EthHash]( mixHashE.simplify );
            nonce           <- RLP.fromEncodable[immutable.Seq[Byte]]( nonceE.simplify )
          } yield {
            EthBlock.Header( 
              parentHash, ommersHash, coinbase, stateRoot, transactionRoot, receiptsRoot, logsBloom,
              difficulty, number, gasLimit, gasUsed, timestamp, extraData, mixHash, nonce
            )
          }
        }
        case other => fail( s"${other} is not in the expected format of an EthBlock.Header" );
      }
    }
  }

  implicit object EthTransactionSeqRLPSerializing extends RLPSerializing.HomogeneousEncodableSeq[EthTransaction];
  implicit object EthBlockHeaderSeqRLPSerializing extends RLPSerializing.HomogeneousEncodableSeq[EthBlock.Header];

  implicit object EthBlockRLPSerializing extends RLPSerializing[EthBlock] {
    def toRLPEncodable( block : EthBlock ) : RLP.Encodable = {
      val txnsSeq = RLP.Encodable.Seq( asEncodables( block.transactions ) );
      val ommersSeq = RLP.Encodable.Seq( asEncodables( block.ommers ) );
      RLP.Encodable.Seq.of( block.header, txnsSeq, ommersSeq );
    }
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : Failable[EthBlock] = {
      import RLP.Encodable.{ByteSeq => BS, Seq => SEQ}
      encodable match {
        case RLP.Encodable.Seq.of( headerBS : BS, txnsSeq : SEQ, ommersSeq : SEQ) => {
          for {
            header <- RLP.fromEncodable[EthBlock.Header]( headerBS );
            txns   <- RLP.fromEncodable[immutable.Seq[EthTransaction]]( txnsSeq.simplify );
            ommers <- RLP.fromEncodable[immutable.Seq[EthBlock.Header]]( ommersSeq.simplify )
          } yield {
            EthBlock( header, txns, ommers )
          }
        }
        case other => fail( s"Expected RLP.Encodable.Seq.of( headerBS : BS, txnsSeq : SEQ, ommersSeq : SEQ), found ${encodable}" )
      }
    }
  }
}




