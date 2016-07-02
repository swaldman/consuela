/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela;

import ethereum.encoding._
import RLPSerializing.asElement  // implicit conversion
import RLPSerializing.asElements // not implicit 

import ethereum.specification.Types.{SignatureV, SignatureR, SignatureS, ByteSeqMax1024, ByteSeqExact20, ByteSeqExact32, ByteSeqExact256, Unsigned64, Unsigned256, Unsigned2048}

import com.mchange.sc.v1.consuela.hash.Keccak256
import com.mchange.sc.v1.consuela.bloom.BitSetBloom

import com.mchange.sc.v2.failable._

import scala.collection._

import scala.util.Try

import com.mchange.sc.v1.log.MLevel._

package object ethereum {
  implicit lazy val logger = mlogger( this )

  class EthereumException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class UnexpectedSignatureFormatException( message : String, t : Throwable = null ) extends EthereumException( message, t );

  type EthHash    = Keccak256;
  val  EthHash    = Keccak256;
  val  EthHashLen = Keccak256.HashLength;

  val EmptyByteSeqHash = EthHash.hash( encoding.RLP.Encoded.EmptyByteSeq );
  val AllZeroesEthHash = EthHash.withBytes( Array.ofDim[Byte]( EthHashLen ) );

  final object Homestead {
    val StartBlock = 1150000
    val LimitSignatureS = specification.Types.Limit.SignatureR / 2
  }

  implicit final object EthHash_RLPSerializing extends RLPSerializing.ByteArrayValue[EthHash]( EthHash.withBytes );

  // I'm not sure why the compiler fails to find, requires me to supply, the RLPSerializing implicit parameter explicitly here
  implicit final object EthLogBloomDefinition extends EthBloom.Definition[EthLogEntry]( (entry : EthLogEntry) => EthHash.hash( RLP.encode( entry )( EthLogEntry_RLPSerializing ) ) );

  final object EthLogBloom {
    def fromBytes( bytes : Array[Byte] ) : EthLogBloom = BitSetBloom.fromBytes[EthLogEntry]( bytes );
    val empty                            : EthLogBloom = BitSetBloom.empty[EthLogEntry];
  }
  type EthLogBloom = BitSetBloom[EthLogEntry];

  implicit class EthLogBloomOps( val elb : EthLogBloom ) extends AnyVal {
    def toByteSeqExact256 : ByteSeqExact256 = ByteSeqExact256( elb.bytes );
  }

  implicit final object EthLogBloom_RLPSerializing extends RLPSerializing[EthLogBloom] {
    def toElement( elb : EthLogBloom ) : RLP.Element = RLP.toElement[ByteSeqExact256]( elb.toByteSeqExact256 );
    def fromElement( element : RLP.Element.Basic ) : Failable[EthLogBloom] = {
      RLP.fromElement[ByteSeqExact256]( element ).map( bse256 => BitSetBloom.fromBytes[EthLogEntry]( bse256.widen ) );
    }
  }

  implicit final object EthAddress_RLPSerializing extends RLPSerializing[EthAddress] {
    def toElement( addr : EthAddress ) : RLP.Element = RLP.Element.ByteSeq( addr.bytes.widen );
    def fromElement( element : RLP.Element.Basic ) : Failable[EthAddress] = {
      element match {
        case RLP.Element.ByteSeq( rawbytes ) => Try( EthAddress( ByteSeqExact20( rawbytes ) ) ).toFailable;
        case _                               => failNotLeaf( element );
      }
    }
  }

  //implicit final object EthAddress_RLPSerializing extends RLPSerializing[EthAddress] {
  //  def toElement( address : EthAddress ) : RLP.Element = RLP.toElement[ByteSeqExact20]( address.toByteSeqExact20 );
  //  def fromElement( element : RLP.Element.Basic ) : Failable[EthAddress] = RLP.fromElement[ByteSeqExact20]( element ).map( EthAddress( _ ) );
  //}

  implicit final object EthTransaction_RLPSerializing extends RLPSerializing[EthTransaction] {
    import EthTransaction._;

    override def toElement( txn : EthTransaction ): RLP.Element = {
      import RLP.{toElement => elem}

      def baseElements( unsigned : Unsigned ) : Vector[RLP.Element] = {
        val (rlpMbTo, payload) = unsigned match {
          case msg : Unsigned.Message          => (msg.to.bytes.widen, msg.data);
          case cc  : Unsigned.ContractCreation => (immutable.Seq.empty[Byte], cc.init);
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
          case RLP.Element.ByteSeq( mbToBytes ) => Try( if (mbToBytes.isEmpty) None else Some( EthAddress( ByteSeqExact20( mbToBytes ) ) ) ).toFailable;
          case whatever                         => failNotLeaf( whatever );
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

  implicit final object EthWorldStateAccount_RLPSerializing extends RLPSerializing[EthWorldState.Account] {
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

  implicit final object EthBlockHeader_RLPSerializing extends RLPSerializing[EthBlock.Header] {
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
            logsBloom       <- RLP.fromElement[EthLogBloom]( logsBloomE.simplify );
            difficulty      <- RLP.fromElement[Unsigned256]( difficultyE.simplify );
            number          <- RLP.fromElement[Unsigned256]( numberE.simplify );
            gasLimit        <- RLP.fromElement[Unsigned256]( gasLimitE.simplify );
            gasUsed         <- RLP.fromElement[Unsigned256]( gasUsedE.simplify );
            timestamp       <- RLP.fromElement[Unsigned256]( timestampE.simplify );
            extraData       <- RLP.fromElement[ByteSeqMax1024]( extraDataE.simplify );
            mixHash         <- RLP.fromElement[EthHash]( mixHashE.simplify );
            nonce           <- RLP.fromElement[Unsigned64]( nonceE.simplify )
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

  implicit final object EthTransactionSeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthTransaction];
  implicit final object EthBlockHeaderSeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthBlock.Header];

  implicit final object EthBlock_RLPSerializing extends RLPSerializing[EthBlock] {
    def toElement( block : EthBlock ) : RLP.Element = {
      val txnsSeq = RLP.Element.Seq( asElements( block.transactions ) );
      val ommersSeq = RLP.Element.Seq( asElements( block.ommers ) );
      RLP.Element.Seq.of( block.header, txnsSeq, ommersSeq );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[EthBlock] = {
      element match {
        case RLP.Element.Seq.of( headerE, txnsSeqE, ommersSeqE ) => {
          for {
            header <- RLP.fromElement[EthBlock.Header]( headerE.simplify );
            txns   <- RLP.fromElement[immutable.Seq[EthTransaction]]( txnsSeqE.simplify );
            ommers <- RLP.fromElement[immutable.Seq[EthBlock.Header]]( ommersSeqE.simplify )
          } yield {
            EthBlock( header, txns, ommers )
          }
        }
        case other => fail( s"Expected RLP.Element.Seq.of( headerE, txnsSeqE, ommersSeqE), found ${element}" )
      }
    }
  }

  implicit final object ByteSeqExact32Seq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[ByteSeqExact32];

  implicit final object EthLogEntry_RLPSerializing extends RLPSerializing[EthLogEntry] {
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

  implicit final object EthLogEntrySeq_RLPSerializing extends RLPSerializing.HomogeneousElementSeq[EthLogEntry];

  implicit final object EthTransactionReceipt_RLPSerializing extends RLPSerializing[EthTransactionReceipt] {
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




