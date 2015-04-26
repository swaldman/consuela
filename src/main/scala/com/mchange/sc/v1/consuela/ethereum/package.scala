package com.mchange.sc.v1.consuela;

import ethereum.encoding._;
import RLPSerializing.asElement;  // implicit conversion
import RLPSerializing.asElements; // not implicit 

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

    override def toElement( txn : EthTransaction ): RLP.Element = {
      import RLP.Element.{UnsignedBigInt => UBI, ByteSeq => BS, UnsignedInt => UI};

      def baseElements( unsigned : Unsigned ) : Vector[RLP.Element] = {
        val (rlpMbTo, payload) = unsigned match {
          case msg : Unsigned.Message          => (msg.to.bytes, msg.data);
          case cc  : Unsigned.ContractCreation => (Nil, cc.init);
        }
        Vector( UBI( unsigned.nonce ), UBI( unsigned.gasPrice ), UBI( unsigned.gasLimit ), BS( rlpMbTo ), UBI( unsigned.value ), BS( payload ) );
      }
      def sigElements( signed : Signed ) : Vector[RLP.Element] = Vector( UI( signed.v ), UBI( signed.r ), UBI( signed.s ) ) 

      txn match {
        case unsigned : Unsigned => RLP.Element.Seq( baseElements( unsigned ) );
        case signed   : Signed   => RLP.Element.Seq( baseElements( signed.base ) ++ sigElements( signed ) );
        case other               => throw new AssertionError( s"Huh? Saw an EthTransaction that is marked neither Signed nor Unsigned: ${other}" );
      }
    }
    override def fromElement( element : RLP.Element.Basic ) : Failable[EthTransaction] = {
      def fromMbToElement( mbToElement : RLP.Element.Basic ) : Failable[Option[EthAddress]] = {
        mbToElement match {
          case RLP.Element.ByteSeq( mbToBytes ) => Try( if (mbToBytes == Nil) None else Some( EthAddress( mbToBytes ) ) ).toFailable;
          case whatever                           => failNotLeaf( whatever );
        }
      }
      element match {
        case RLP.Element.Seq.of( nonceE, gasPriceE, gasLimitE, mbToE, valueE, payloadE, rest @ _* ) => {
          val base = for {
            nonce    <- RLP.fromElement[BigInt]( nonceE.simplify );
            gasPrice <- RLP.fromElement[BigInt]( gasPriceE.simplify );
            gasLimit <- RLP.fromElement[BigInt]( gasLimitE.simplify );
            mbTo     <- fromMbToElement( mbToE.simplify );
            value    <- RLP.fromElement[BigInt]( valueE.simplify );
            payload  <- RLP.fromElement[immutable.Seq[Byte]]( payloadE.simplify )
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
                  v        <- RLP.fromElement[Int]( vE.simplify );
                  r        <- RLP.fromElement[BigInt]( rE.simplify );
                  s        <- RLP.fromElement[BigInt]( sE.simplify );
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
    def toElement( account : WorldState.Account ) : RLP.Element = {
      val codeHash = {
        account match {
          case contract : WorldState.Account.Contract => contract.codeHash;
          case agent    : WorldState.Account.Agent    => trie.EmptyTrieHash;
        }
      }

      import account._;
      RLP.Element.Seq.of( nonce, balance, storageRoot, codeHash );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[WorldState.Account] = {
      element match {
        case RLP.Element.Seq.of( nonceE , balanceE, storageRootE, codeHashE ) => {
          for {
            nonce       <- RLP.fromElement[BigInt]( nonceE.simplify );
            balance     <- RLP.fromElement[BigInt]( balanceE.simplify );
            storageRoot <- RLP.fromElement[EthHash]( storageRootE.simplify );
            codeHash    <- RLP.fromElement[EthHash]( codeHashE.simplify )
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
    def toElement( header : EthBlock.Header ) : RLP.Element = {
      import header._
      RLP.Element.Seq.of( 
        parentHash, ommersHash, coinbase, stateRoot, transactionRoot, receiptsRoot, logsBloom,
        difficulty, number, gasLimit, gasUsed, timestamp, extraData, mixHash, nonce
      )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthBlock.Header] = {
      element match {
        case RLP.Element.Seq.of(
          parentHashE, ommersHashE, coinbaseE, stateRootE, transactionRootE, receiptsRootE, logsBloomE,
          difficultyE, numberE, gasLimitE, gasUsedE, timestampE, extraDataE, mixHashE, nonceE
        ) => {
          for {
            parentHash      <- RLP.fromElement[EthHash]( parentHashE.simplify );
            ommersHash      <- RLP.fromElement[EthHash]( ommersHashE.simplify );
            coinbase        <- RLP.fromElement[EthAddress]( coinbaseE.simplify );
            stateRoot       <- RLP.fromElement[EthHash]( stateRootE.simplify );
            transactionRoot <- RLP.fromElement[EthHash]( transactionRootE.simplify )
            receiptsRoot    <- RLP.fromElement[EthHash]( receiptsRootE.simplify );
            logsBloom       <- RLP.fromElement[BigInt]( logsBloomE.simplify );
            difficulty      <- RLP.fromElement[BigInt]( difficultyE.simplify );
            number          <- RLP.fromElement[BigInt]( numberE.simplify );
            gasLimit        <- RLP.fromElement[BigInt]( gasLimitE.simplify );
            gasUsed         <- RLP.fromElement[BigInt]( gasUsedE.simplify );
            timestamp       <- RLP.fromElement[BigInt]( timestampE.simplify );
            extraData       <- RLP.fromElement[immutable.Seq[Byte]]( extraDataE.simplify );
            mixHash         <- RLP.fromElement[EthHash]( mixHashE.simplify );
            nonce           <- RLP.fromElement[immutable.Seq[Byte]]( nonceE.simplify )
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

  implicit object EthTransactionSeqRLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthTransaction];
  implicit object EthBlockHeaderSeqRLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthBlock.Header];

  implicit object EthBlockRLPSerializing extends RLPSerializing[EthBlock] {
    def toElement( block : EthBlock ) : RLP.Element = {
      val txnsSeq = RLP.Element.Seq( asElements( block.transactions ) );
      val ommersSeq = RLP.Element.Seq( asElements( block.ommers ) );
      RLP.Element.Seq.of( block.header, txnsSeq, ommersSeq );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthBlock] = {
      import RLP.Element.{ByteSeq => BS, Seq => SEQ}
      element match {
        case RLP.Element.Seq.of( headerBS : BS, txnsSeq : SEQ, ommersSeq : SEQ) => {
          for {
            header <- RLP.fromElement[EthBlock.Header]( headerBS );
            txns   <- RLP.fromElement[immutable.Seq[EthTransaction]]( txnsSeq.simplify );
            ommers <- RLP.fromElement[immutable.Seq[EthBlock.Header]]( ommersSeq.simplify )
          } yield {
            EthBlock( header, txns, ommers )
          }
        }
        case other => fail( s"Expected RLP.Element.Seq.of( headerBS : BS, txnsSeq : SEQ, ommersSeq : SEQ), found ${element}" )
      }
    }
  }
}




