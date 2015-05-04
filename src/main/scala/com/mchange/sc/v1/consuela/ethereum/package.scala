package com.mchange.sc.v1.consuela;

import ethereum.encoding._;
import RLPSerializing.asElement;  // implicit conversion
import RLPSerializing.asElements; // not implicit 

import ethereum.specification.Types.{SignatureV, SignatureR, SignatureS, ByteSeqMax1024,ByteSeqExact8, ByteSeqExact20, ByteSeqExact32, ByteSeqExact256, Unsigned256, Unsigned2048}

import com.mchange.sc.v1.consuela.hash.Hash;
import com.mchange.sc.v1.consuela.bloom.BitSetBloom;

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

  implicit object EthHash_RLPSerializing extends RLPSerializing.ByteArrayValue[EthHash]( EthHash.withBytes );

  // I'm not sure why the compiler fails to find, requires me to supply, the RLPSerializing implicit parameter explicitly here
  implicit object EthLogBloomDefinition extends EthBloom.Definition[EthLogEntry]( (entry : EthLogEntry) => EthHash.hash( RLP.encode( entry )( EthLogEntry_RLPSerializing ) ) );

  type EthLogBloom = BitSetBloom[EthLogEntry];

  implicit class EthLogBloomOps( val elb : EthLogBloom ) extends AnyVal {
    def toByteSeqExact256 : ByteSeqExact256 = ByteSeqExact256( elb.bytes );
  }

  implicit object EthLogBloom_RLPSerializing extends RLPSerializing[EthLogBloom] {
    def toElement( elb : EthLogBloom ) : RLP.Element = RLP.toElement[ByteSeqExact256]( elb.toByteSeqExact256 );
    def fromElement( element : RLP.Element.Basic ) : Failable[EthLogBloom] = {
      RLP.fromElement[ByteSeqExact256]( element ).map( bse256 => BitSetBloom.fromBytes[EthLogEntry]( bse256.widen ) );
    }
  }

  // XXX: should I switch to the more strongly typed version below?
  implicit object EthAddress_RLPSerializing extends RLPSerializing.ByteArrayValue[EthAddress]( EthAddress.apply );

  //implicit object EthAddress_RLPSerializing extends RLPSerializing[EthAddress] {
  //  def toElement( address : EthAddress ) : RLP.Element = RLP.toElement[ByteSeqExact20]( address.toByteSeqExact20 );
  //  def fromElement( element : RLP.Element.Basic ) : Failable[EthAddress] = RLP.fromElement[ByteSeqExact20]( element ).map( EthAddress( _ ) );
  //}

  implicit object EthTransaction_RLPSerializing extends RLPSerializing[EthTransaction] {
    import EthTransaction._;

    override def toElement( txn : EthTransaction ): RLP.Element = {
      import RLP.{toElement => elem}

      def baseElements( unsigned : Unsigned ) : Vector[RLP.Element] = {
        val (rlpMbTo, payload) = unsigned match {
          case msg : Unsigned.Message          => (msg.to.bytes, msg.data);
          case cc  : Unsigned.ContractCreation => (Nil, cc.init);
        }
        Vector( elem( unsigned.nonce ), elem( unsigned.gasPrice ), elem( unsigned.gasLimit ), elem( rlpMbTo ), elem( unsigned.value ), elem( payload ) );
      }
      def sigElements( signed : Signed ) : Vector[RLP.Element] = Vector( elem( signed.v ), elem( signed.r ), elem( signed.s ) ) 

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
            nonce    <- RLP.fromElement[Unsigned256]( nonceE.simplify );
            gasPrice <- RLP.fromElement[Unsigned256]( gasPriceE.simplify );
            gasLimit <- RLP.fromElement[Unsigned256]( gasLimitE.simplify );
            mbTo     <- fromMbToElement( mbToE.simplify );
            value    <- RLP.fromElement[Unsigned256]( valueE.simplify );
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
                  v        <- RLP.fromElement[SignatureV]( vE.simplify );
                  r        <- RLP.fromElement[SignatureR]( rE.simplify );
                  s        <- RLP.fromElement[SignatureS]( sE.simplify );
                  sig      <- Try( EthSignature( v, r, s ) ).toFailable
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

  implicit object EthWorldStateAccount_RLPSerializing extends RLPSerializing[EthWorldState.Account] {
    def toElement( account : EthWorldState.Account ) : RLP.Element = {
      val codeHash = {
        account match {
          case contract : EthWorldState.Account.Contract => contract.codeHash;
          case agent    : EthWorldState.Account.Agent    => trie.EmptyTrieHash;
        }
      }

      import account._;
      RLP.Element.Seq.of( nonce, balance, storageRoot, codeHash );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthWorldState.Account] = {
      element match {
        case RLP.Element.Seq.of( nonceE , balanceE, storageRootE, codeHashE ) => {
          for {
            nonce       <- RLP.fromElement[Unsigned256]( nonceE.simplify );
            balance     <- RLP.fromElement[Unsigned256]( balanceE.simplify );
            storageRoot <- RLP.fromElement[EthHash]( storageRootE.simplify );
            codeHash    <- RLP.fromElement[EthHash]( codeHashE.simplify )
          } yield {
            EthWorldState.Account( nonce, balance, storageRoot, codeHash );
          }
        }
        case other => fail( s"Expected ( nonceE , balanceE, storageRootE, codeHashE ), found ${other}" );
      }
    }
  }

  implicit object EthBlockHeader_RLPSerializing extends RLPSerializing[EthBlock.Header] {
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
            logsBloom       <- RLP.fromElement[Unsigned2048]( logsBloomE.simplify );
            difficulty      <- RLP.fromElement[Unsigned256]( difficultyE.simplify );
            number          <- RLP.fromElement[Unsigned256]( numberE.simplify );
            gasLimit        <- RLP.fromElement[Unsigned256]( gasLimitE.simplify );
            gasUsed         <- RLP.fromElement[Unsigned256]( gasUsedE.simplify );
            timestamp       <- RLP.fromElement[Unsigned256]( timestampE.simplify );
            extraData       <- RLP.fromElement[ByteSeqMax1024]( extraDataE.simplify );
            mixHash         <- RLP.fromElement[EthHash]( mixHashE.simplify );
            nonce           <- RLP.fromElement[ByteSeqExact8]( nonceE.simplify )
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

  implicit object EthTransactionSeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthTransaction];
  implicit object EthBlockHeaderSeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthBlock.Header];

  implicit object EthBlock_RLPSerializing extends RLPSerializing[EthBlock] {
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

  implicit object ByteSeqExact32Seq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[ByteSeqExact32];

  implicit object EthLogEntry_RLPSerializing extends RLPSerializing[EthLogEntry] {
    def toElement( entry : EthLogEntry ) : RLP.Element = {
      import entry._
      RLP.Element.Seq.of( address, topics, data );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthLogEntry] = {
      element match {
        case RLP.Element.Seq.of( addressE, topicsE, dataE ) => {
          for {
            address <- RLP.fromElement[EthAddress]( addressE.simplify );
            topics  <- RLP.fromElement[immutable.Seq[ByteSeqExact32]]( topicsE.simplify );
            data    <- RLP.fromElement[immutable.Seq[Byte]]( dataE.simplify )
          } yield {
            EthLogEntry( address, topics, data )
          }
        }
        case other => fail( s"${other} is not in the expected format of an EthLogEntry" );
      }
    }
  }

  implicit object EthLogEntrySeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthLogEntry];

  implicit object EthTransactionReceipt_RLPSerializing extends RLPSerializing[EthTransactionReceipt] {
    def toElement( receipt : EthTransactionReceipt ) : RLP.Element = {
      import receipt._
      RLP.Element.Seq.of( postTransactionState, gasUsed, logsBloom, logEntries );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthTransactionReceipt] = {
      element match {
        case RLP.Element.Seq.of( postTransactionStateE, gasUsedE, logsBloomE, logEntriesE ) => {
          for {
            postTransactionState <- RLP.fromElement[EthHash]( postTransactionStateE.simplify );
            gasUsed              <- RLP.fromElement[Unsigned256]( gasUsedE.simplify );
            logsBloom            <- RLP.fromElement[EthLogBloom]( logsBloomE.simplify )
            logEntries           <- RLP.fromElement[immutable.Seq[EthLogEntry]]( logEntriesE.simplify )
          } yield {
            EthTransactionReceipt( postTransactionState, gasUsed, logsBloom, logEntries )
          }
        }
        case other => fail( s"${other} is not in the expected format of an EthTransactionReceipt" );
      }
    }
  }
}




