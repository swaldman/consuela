package com.mchange.sc.v1.consuela.ethereum.net

import scala.language.implicitConversions

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v3.failable._

import java.nio.charset.{Charset,StandardCharsets}
import scala.collection.immutable

import RLPSerializing.asElement

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
        case other                  => Failable.fail( s"Subprotocol.P2P4.Hello.Capabilities should be a sequence, found ${other}" )
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
          case other => Failable.fail( s"Individual elements of Subprotocol.P2P4.Hello.Capabilities should be a pair encoded as a sequence, found ${other}" )
        }
      }
      val failables    : immutable.Seq[Failable[(StringASCII_Exact3, Unsigned16)]] = seq.map( decodeTuple );
      val firstFailure : Option[Failed[(StringASCII_Exact3, Unsigned16)]]          = failables.find( _.isFailed ).map( _.asFailed );
      firstFailure.fold( Failable.succeed( failables.map( _.get ) ) )( failed => Failable.refail( failed ) )
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.P2P4.Hello: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.P2P4.Disconnect: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.P2P4.Ping: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.P2P4.Pong: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth60.Status: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth60.GetBlockHashes: ${other}" );
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
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth60.NewBlock: ${other}" );
      }
    }
  }

  //Eth63 RLP
  /*
  implicit final object Subprotocol_Eth63_Undefined extends RLPSerializing[Subprotocol.Eth63.Undefined] {
    def toElement( status : Subprotocol.Eth63.Undefined ) : RLP.Element = {
      import status._
      RLP.Element.Seq.of( typeCode )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.Undefined] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned]( typeCodeE.simplify )
          } yield {
            Subprotocol.Eth63.Undefined( typeCode )
          }
        }
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth63.Udnefined: ${other}" );
      }
    }
  }
  */ 
  implicit final object Subprotocol_Eth63_Status extends RLPSerializing[Subprotocol.Eth63.Status] {
    def toElement( status : Subprotocol.Eth63.Status ) : RLP.Element = {
      import status._
      RLP.Element.Seq.of( typeCode, protocolVersion, networkId, totalDifficulty, bestHash, genesisHash );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.Status] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, protocolVersionE, networkIdE, totalDifficultyE, bestHashE, genesisHashE, numberE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            protocolVersion <- RLP.fromElement[Unsigned]  ( protocolVersionE.simplify )
            networkId       <- RLP.fromElement[Unsigned]  ( networkIdE.simplify )
            totalDifficulty <- RLP.fromElement[Unsigned]  ( totalDifficultyE.simplify )
            bestHash        <- RLP.fromElement[EthHash]   ( bestHashE.simplify )
            genesisHash     <- RLP.fromElement[EthHash]   ( genesisHashE.simplify )
            number          <- RLP.fromElement[Unsigned]  ( numberE.simplify )
          } yield {
            Subprotocol.Eth63.Status( typeCode, protocolVersion, networkId, totalDifficulty, bestHash, genesisHash, number )
          }
        }
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth63.Status: ${other}" );
      }
    }
  }
  implicit def rlpSerialzingTuple2[A : RLPSerializing, B : RLPSerializing] : RLPSerializing[Tuple2[A,B]] = new RLPSerializing[Tuple2[A,B]] {
    def toElement( tup : Tuple2[A,B] ) : RLP.Element = RLP.Element.Seq( RLP.toElement( tup._1 ) :: RLP.toElement( tup._2 ) :: Nil )
    def fromElement( element : RLP.Element.Basic ) : Failable[Tuple2[A,B]] = {
      element match {
        case RLP.Element.Seq.of( _a, _b ) => {
          for {
            a <- RLP.fromElement[A](_a.simplify)
            b <- RLP.fromElement[B](_b.simplify)
          }
          yield {
            Tuple2(a,b)
          }
        }
        case other => Failable.fail( s"Unexpected structured for RLP-serialized Tuple2: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth63_NewBlockHashes extends RLPSerializing[Subprotocol.Eth63.NewBlockHashes] {
    def toElement( nbh : Subprotocol.Eth63.NewBlockHashes ) : RLP.Element = toTypeCodeSeqElement( nbh.typeCode, nbh.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.NewBlockHashes] = {
      fromTypeCodeSeqElement[Tuple2[EthHash,Unsigned]]( element, "Subprotocol.Eth63.NewBlockHashes" ).map( pair => Subprotocol.Eth63.NewBlockHashes( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_Transactions extends RLPSerializing[Subprotocol.Eth63.Transactions] {
    def toElement( txns : Subprotocol.Eth63.Transactions ) : RLP.Element = toTypeCodeSeqElement( txns.typeCode, txns.transactions );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.Transactions] = {
      fromTypeCodeSeqElement[EthTransaction]( element, "Subprotocol.Eth63.Transactions" ).map( pair => Subprotocol.Eth63.Transactions( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_GetBlockHeaders extends RLPSerializing[Subprotocol.Eth63.GetBlockHeaders] {
    def toElement( gbh : Subprotocol.Eth63.GetBlockHeaders ) : RLP.Element = {
      import gbh._
      val blockE = {
        block match {
          case Left( hash )      => RLP.toElement( hash )
          case Right( blockNum ) => RLP.toElement( blockNum )
        }
      }
      RLP.Element.Seq.of( typeCode, blockE, maxHeaders, skip, reverse );
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.GetBlockHeaders] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, blockE, maxHeadersE, skipE, reverseE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            block           <- RLP.fromElement[EthHash]( blockE.simplify ).map( Left.apply ) orElseTrace RLP.fromElement[Unsigned]( blockE.simplify ).map( Right.apply )
            maxHeaders      <- RLP.fromElement[Unsigned]  ( maxHeadersE.simplify )
            skip            <- RLP.fromElement[Unsigned]  ( skipE.simplify )
            reverse         <- RLP.fromElement[Unsigned1] ( reverseE.simplify )
          } yield {
            Subprotocol.Eth63.GetBlockHeaders( typeCode, block, maxHeaders, skip, reverse )
          }
        }
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth63.GetBlockHeaders: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth63_BlockHeaders extends RLPSerializing[Subprotocol.Eth63.BlockHeaders] {
    def toElement( bh : Subprotocol.Eth63.BlockHeaders ) : RLP.Element = toTypeCodeSeqElement( bh.typeCode, bh.blockHeaders );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.BlockHeaders] = {
      fromTypeCodeSeqElement[EthBlock.Header]( element, "Subprotocol.Eth63.BlockHeaders" ).map( pair => Subprotocol.Eth63.BlockHeaders( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_GetBlockBodies extends RLPSerializing[Subprotocol.Eth63.GetBlockBodies] {
    def toElement( gbb : Subprotocol.Eth63.GetBlockBodies ) : RLP.Element = toTypeCodeSeqElement( gbb.typeCode, gbb.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.GetBlockBodies] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth63.GetBlockBodies" ).map( pair => Subprotocol.Eth63.GetBlockBodies( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_BlockBodies extends RLPSerializing[Subprotocol.Eth63.BlockBodies] {
    def toElement( bb : Subprotocol.Eth63.BlockBodies ) : RLP.Element = toTypeCodeSeqElement( bb.typeCode, bb.bodies );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.BlockBodies] = {
      fromTypeCodeSeqElement[Tuple2[immutable.Seq[EthTransaction],immutable.Seq[EthBlock.Header]]]( element, "Subprotocol.Eth63.BlockBodies" ).map( pair => Subprotocol.Eth63.BlockBodies( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_NewBlock extends RLPSerializing[Subprotocol.Eth63.NewBlock] {
    def toElement( nb : Subprotocol.Eth63.NewBlock ) : RLP.Element = {
      import nb._
      RLP.Element.Seq.of( typeCode, block, totalDifficulty )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.NewBlock] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, blockE, totalDifficultyE ) => {
          for {
            typeCode        <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            block           <- RLP.fromElement[EthBlock]  ( blockE.simplify )
            totalDifficulty <- RLP.fromElement[Unsigned]  ( totalDifficultyE.simplify )
          } yield {
            Subprotocol.Eth63.NewBlock( typeCode, block, totalDifficulty )
          }
        }
        case other => Failable.fail( s"Unexpected element for Subprotocol.Eth63.NewBlock: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth63_GetNodeData extends RLPSerializing[Subprotocol.Eth63.GetNodeData] {
    def toElement( gnd : Subprotocol.Eth63.GetNodeData ) : RLP.Element = toTypeCodeSeqElement( gnd.typeCode, gnd.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.GetNodeData] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth63.GetNodeData" ).map( pair => Subprotocol.Eth63.GetNodeData( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_NodeData extends RLPSerializing[Subprotocol.Eth63.NodeData] {
    def toElement( nd : Subprotocol.Eth63.NodeData ) : RLP.Element = toTypeCodeSeqElement( nd.typeCode, nd.values );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.NodeData] = {
      fromTypeCodeSeqElement[immutable.Seq[Byte]]( element, "Subprotocol.Eth63.NodeData" ).map( pair => Subprotocol.Eth63.NodeData( pair._1, pair._2 ) )
    }
  }
  implicit final object Subprotocol_Eth63_GetReceipts extends RLPSerializing[Subprotocol.Eth63.GetReceipts] {
    def toElement( gr : Subprotocol.Eth63.GetReceipts ) : RLP.Element = toTypeCodeSeqElement( gr.typeCode, gr.hashes );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.GetReceipts] = {
      fromTypeCodeSeqElement[EthHash]( element, "Subprotocol.Eth63.GetReceipts" ).map( pair => Subprotocol.Eth63.GetReceipts( pair._1, pair._2 ) )
    }
  }
  implicit final object ReceiptSeq extends RLPSerializing[immutable.Seq[EthTransactionReceipt]] {
    def toElement( rs : immutable.Seq[EthTransactionReceipt] ) : RLP.Element = RLP.Element.Seq( rs.map( RLP.toElement[EthTransactionReceipt] ) )
    def fromElement( element : RLP.Element.Basic ) : Failable[immutable.Seq[EthTransactionReceipt]] = {
      element match {
        case RLP.Element.Seq( elements ) => Failable.sequence( elements.map( elem => RLP.fromElement[EthTransactionReceipt]( elem.simplify ) ) )
        case other => Failable.fail( s"Unexpected element for immutable.Seq[EthTransactionReceipt]: ${other}" );
      }
    }
  }
  implicit final object ReceiptSeqSeq extends RLPSerializing[immutable.Seq[immutable.Seq[EthTransactionReceipt]]] {
    def toElement( rss : immutable.Seq[immutable.Seq[EthTransactionReceipt]] ) : RLP.Element = RLP.Element.Seq( rss.map( RLP.toElement[immutable.Seq[EthTransactionReceipt]] ) )
    def fromElement( element : RLP.Element.Basic ) : Failable[immutable.Seq[immutable.Seq[EthTransactionReceipt]]] = {
      element match {
        case RLP.Element.Seq( elements ) => Failable.sequence( elements.map( elem => RLP.fromElement[immutable.Seq[EthTransactionReceipt]]( elem.simplify ) ) )
        case other => Failable.fail( s"Unexpected element for immutable.Seq[immutable.Seq[EthTransactionReceipt]]: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth63_Receipts extends RLPSerializing[Subprotocol.Eth63.Receipts] {
    def toElement( r : Subprotocol.Eth63.Receipts ) : RLP.Element = toTypeCodeSeqElement( r.typeCode, r.receiptLists );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth63.Receipts] = {
      fromTypeCodeSeqElement[immutable.Seq[EthTransactionReceipt]]( element, "Subprotocol.Eth63.Receipts" ).map( pair => Subprotocol.Eth63.Receipts( pair._1, pair._2 ) )
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
          val failableSeq = mbFirstFailure.fold( Failable.succeed( failables.map( _.get ) ) )( failable => Failable.refail( failable.asFailed ) )
          failableSeq.map( seq => ( typeCode, immutable.IndexedSeq( seq : _* ) ) )
        }
      }
      case other => Failable.fail( s"Unexpected element for ${className}: ${other}" );
    }
  }

  /*
  private def toUnrestrictedTypeCodeSeqElement[T : RLPSerializing]( typeCode : Unsigned, ts : Seq[T] ) : RLP.Element = {
    RLP.Element.Seq( RLP.toElement( typeCode ) +: ts.map( RLP.toElement( _ ) ) )
  }
  private def fromUnrestrictedTypeCodeSeqElement[T : RLPSerializing]( element : RLP.Element.Basic, className : String ) : Failable[ (Unsigned, immutable.IndexedSeq[T]) ] = {
    element match {
      case RLP.Element.Seq.of( typeCodeE, hashEs @ _* ) => {
        RLP.fromElement[Unsigned]( typeCodeE.simplify ).flatMap{ typeCode =>
          val failables = hashEs.map( hashE => RLP.fromElement[T]( hashE.simplify ) );
          val mbFirstFailure = failables.find( _.isFailed )
          val failableSeq = mbFirstFailure.fold( Failable.succeed( failables.map( _.get ) ) )( failable => Failable.refail( failable.asFailed ) )
          failableSeq.map( seq => ( typeCode, immutable.IndexedSeq( seq : _* ) ) )
        }
      }
      case other => Failable.fail( s"Unexpected element for ${className}: ${other}" );
    }
  }
  */ 
}
