package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.failable._

import java.nio.charset.{Charset,StandardCharsets}
import scala.collection.immutable;

import RLPSerializing.asElement;

package object devp2p {
  //type AnyPayload = P forSome { type P <: Payload[P] }

  // P2P4 protocol RLP
  implicit final object Subprotocol_P2P4_Hello_Capabilities extends RLPSerializing[Subprotocol.P2P4.Hello.Capabilities] {
    def toElement( rlpSerializable : Subprotocol.P2P4.Hello.Capabilities ) : RLP.Element = {
      RLP.Element.Seq( rlpSerializable.elements.toSeq.map( pair => RLP.Element.Seq.of( RLP.toElement(pair._1), RLP.toElement(pair._2) ) ) )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.P2P4.Hello.Capabilities] = {
      element match {
        case RLP.Element.Seq( seq ) => decodeTuples( seq ).map( tupleSeq => Subprotocol.P2P4.Hello.Capabilities( immutable.Set( tupleSeq : _* ) ) )
        case other                  => fail( s"Subprotocol.P2P4.Hello.Capabilities should be a sequence, found ${other}" )
      }
    }
    private def decodeTuples( seq : immutable.Seq[RLP.Element] ) : Failable[Seq[(StringASCII_Exact3, Unsigned16)]] = {
      def decodeTuple( elem : RLP.Element ) : Failable[(StringASCII_Exact3, Unsigned16)] = {
        elem match {
          case RLP.Element.Seq.of( capNameE, capVersionE ) => {
            for {
              capName    <- RLP.fromElement[StringASCII_Exact3](capNameE.simplify)
              capVersion <- RLP.fromElement[Unsigned16](capVersionE.simplify)
            } yield {
              ( capName, capVersion )
            }
          }
          case other => fail( s"Individual elements of Subprotocol.P2P4.Hello.Capabilities should be a pair encoded as a sequence, found ${other}" )
        }
      }
      val failables    : immutable.Seq[Failable[(StringASCII_Exact3, Unsigned16)]] = seq.map( decodeTuple );
      val firstFailure : Option[Failed[(StringASCII_Exact3, Unsigned16)]]          = failables.find( _.isFailed ).map( _.asFailed );
      firstFailure.fold( succeed( failables.map( _.get ) ) )( failed => refail( failed ) )
    }
  }
  implicit final object Subprotocol_P2P4_Hello_RLPSerialzing extends RLPSerializing[Subprotocol.P2P4.Hello] {
    def toElement( rlpSerializable : Subprotocol.P2P4.Hello ) : RLP.Element = {
      import rlpSerializable._
      RLP.Element.Seq.of(
        typeCode,
        p2pVersion,
        clientId,
        capabilities,
        listenPort,
        nodeId
      )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.P2P4.Hello] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, p2pVersionE, clientIdE, capabilitiesE, listenPortE, nodeIdE ) => {
          for {
            typeCode     <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            p2pVersion   <- RLP.fromElement[Unsigned16]( p2pVersionE.simplify )
            clientId     <- RLP.fromElement[StringUTF8]( clientIdE.simplify )
            capabilities <- RLP.fromElement[Subprotocol.P2P4.Hello.Capabilities]( capabilitiesE.simplify )
            listenPort   <- RLP.fromElement[Unsigned16]( listenPortE.simplify )
            nodeId       <- RLP.fromElement[ByteSeqExact64]( nodeIdE.simplify )
          } yield {
            Subprotocol.P2P4.Hello( typeCode, p2pVersion, clientId, capabilities, listenPort, nodeId )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.P2P4.Hello: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_P2P4_Disconnect extends RLPSerializing[Subprotocol.P2P4.Disconnect] {
    def toElement( disconnect : Subprotocol.P2P4.Disconnect ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode, disconnect.reason );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.P2P4.Disconnect] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, reasonE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            reason   <- RLP.fromElement[Unsigned16]( reasonE.simplify )
          } yield {
            Subprotocol.P2P4.Disconnect( typeCode, reason )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.P2P4.Disconnect: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_P2P4_Ping extends RLPSerializing[Subprotocol.P2P4.Ping] {
    def toElement( disconnect : Subprotocol.P2P4.Ping ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.P2P4.Ping] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.P2P4.Ping( typeCode )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.P2P4.Ping: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_P2P4_Pong extends RLPSerializing[Subprotocol.P2P4.Pong] {
    def toElement( disconnect : Subprotocol.P2P4.Pong ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.P2P4.Pong] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.P2P4.Pong( typeCode )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.P2P4.Pong: ${other}" );
      }
    }
  }

  //Eth60 RLP
  implicit final object Subprotocol_Eth60_Status extends RLPSerializing[Subprotocol.Eth60.Status] {
    def toElement( status : Subprotocol.Eth60.Status ) : RLP.Element = {
      import status._
      RLP.Element.Seq.of( typeCode, protocolVersion, networkId, totalDifficulty, bestHash, genesisHash );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.Status] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, protocolVersionE, networkIdE, totalDifficultyE, bestHashE, genesisHashE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned16] ( typeCodeE.simplify )
            protocolVersion <- RLP.fromElement[Unsigned16] ( protocolVersionE.simplify )
            networkId       <- RLP.fromElement[Unsigned1]  ( networkIdE.simplify )
            totalDifficulty <- RLP.fromElement[Unsigned256]( totalDifficultyE.simplify )
            bestHash        <- RLP.fromElement[EthHash]    ( bestHashE.simplify )
            genesisHash     <- RLP.fromElement[EthHash]    ( genesisHashE.simplify )
          } yield {
            Subprotocol.Eth60.Status( typeCode, protocolVersion, networkId, totalDifficulty, bestHash, genesisHash  )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.Eth60.Status: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth60_NewBlockHashes extends RLPSerializing[Subprotocol.Eth60.NewBlockHashes] {
    def toElement( nbh : Subprotocol.Eth60.NewBlockHashes ) : RLP.Element = toTypeCodeSeqElement( nbh.typeCode, nbh.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.NewBlockHashes] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth60.NewBlockHashes" ).map( pair => Subprotocol.Eth60.NewBlockHashes( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth60_Transactions extends RLPSerializing[Subprotocol.Eth60.Transactions] {
    def toElement( txns : Subprotocol.Eth60.Transactions ) : RLP.Element = toTypeCodeSeqElement( txns.typeCode, txns.transactions );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.Transactions] = {
      fromTypeCodeSeqElement[EthTransaction]( element, "Subprotocol.Eth60.Transactions" ).map( pair => Subprotocol.Eth60.Transactions( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth60_GetBlockHashes extends RLPSerializing[Subprotocol.Eth60.GetBlockHashes] {
    def toElement( gbh : Subprotocol.Eth60.GetBlockHashes ) : RLP.Element = {
      import gbh._
      RLP.Element.Seq.of( typeCode, hash, maxBlocks )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.GetBlockHashes] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, hashE, maxBlocksE ) => {
          for {
            typeCode  <- RLP.fromElement[Unsigned16]  ( typeCodeE.simplify )
            hash      <- RLP.fromElement[EthHash]     ( hashE.simplify )
            maxBlocks <- RLP.fromElement[Unsigned256] ( maxBlocksE.simplify )
          } yield {
            Subprotocol.Eth60.GetBlockHashes( typeCode, hash, maxBlocks )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.Eth60.GetBlockHashes: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth60_BlockHashes extends RLPSerializing[Subprotocol.Eth60.BlockHashes] {
    def toElement( bh : Subprotocol.Eth60.BlockHashes ) : RLP.Element = toTypeCodeSeqElement( bh.typeCode, bh.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.BlockHashes] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth60.BlockHashes" ).map( pair => Subprotocol.Eth60.BlockHashes( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth60_GetBlocks extends RLPSerializing[Subprotocol.Eth60.GetBlocks] {
    def toElement( gb : Subprotocol.Eth60.GetBlocks ) : RLP.Element = toTypeCodeSeqElement( gb.typeCode, gb.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.GetBlocks] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth60.GetBlocks" ).map( pair => Subprotocol.Eth60.GetBlocks( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth60_Blocks extends RLPSerializing[Subprotocol.Eth60.Blocks] {
    def toElement( bs : Subprotocol.Eth60.Blocks ) : RLP.Element = toTypeCodeSeqElement( bs.typeCode, bs.blocks );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.Blocks] = {
      fromTypeCodeSeqElement[EthBlock]( element, "Subprotocol.Eth60.Blocks" ).map( pair => Subprotocol.Eth60.Blocks( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth60_NewBlock extends RLPSerializing[Subprotocol.Eth60.NewBlock] {
    def toElement( nb : Subprotocol.Eth60.NewBlock ) : RLP.Element = RLP.Element.Seq.of( nb.typeCode, nb.block, nb.totalDifficulty )
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.NewBlock] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, blockE, totalDifficultyE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned16]  ( typeCodeE.simplify )
            block           <- RLP.fromElement[EthBlock]    ( blockE.simplify )
            totalDifficulty <- RLP.fromElement[Unsigned256] ( totalDifficultyE.simplify )
          } yield {
            Subprotocol.Eth60.NewBlock( typeCode, block, totalDifficulty )
          }
        }
        case other => fail( s"Unexpected element for Subprotocol.Eth60.NewBlock: ${other}" );
      }
    }
  }

  // utilities

  private def toTypeCodeSeqElement[T : RLPSerializing]( typeCode : Unsigned16, ts : Seq[T] ) : RLP.Element = {
    RLP.Element.Seq( RLP.toElement( typeCode ) +: ts.map( RLP.toElement( _ ) ) )
  }
  private def fromTypeCodeSeqElement[T : RLPSerializing]( element : RLP.Element.Basic, className : String ) : Failable[ (Unsigned16, immutable.IndexedSeq[T]) ] = {
    element match {
      case RLP.Element.Seq.of( typeCodeE, hashEs @ _* ) => {
        RLP.fromElement[Unsigned16]( typeCodeE.simplify ).flatMap{ typeCode =>
          val failables = hashEs.map( hashE => RLP.fromElement[T]( hashE.simplify ) );
          val mbFirstFailure = failables.find( _.isFailed )
          val failableSeq = mbFirstFailure.fold( succeed( failables.map( _.get ) ) )( failable => refail( failable.asFailed ) )
          failableSeq.map( seq => ( typeCode, immutable.IndexedSeq( seq : _* ) ) )
        }
      }
      case other => fail( s"Unexpected element for ${className}: ${other}" );
    }
  }
}
