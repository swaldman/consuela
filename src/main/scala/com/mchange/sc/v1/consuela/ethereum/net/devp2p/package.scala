package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.failable._

import java.nio.charset.{Charset,StandardCharsets}
import scala.collection.immutable;

import RLPSerializing.asElement;

package object devp2p {

  // Core protocol RLP
  implicit final object Subprotocol_Core_Hello_Capabilities extends RLPSerializing[Subprotocol.Core.Hello.Capabilities] {
    def toElement( rlpSerializable : Subprotocol.Core.Hello.Capabilities ) : RLP.Element = {
      RLP.Element.Seq( rlpSerializable.elements.toSeq.map( pair => RLP.Element.Seq.of( RLP.toElement(pair._1), RLP.toElement(pair._2) ) ) )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Core.Hello.Capabilities] = {
      element match {
        case RLP.Element.Seq( seq ) => decodeTuples( seq ).map( tupleSeq => Subprotocol.Core.Hello.Capabilities( immutable.Set( tupleSeq : _* ) ) )
        case other                  => fail( s"Subprotocol.Core.Hello.Capabilities should be a sequence, found ${other}" )
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
          case other => fail( s"Individual elements of Subprotocol.Core.Hello.Capabilities should be a pair encoded as a sequence, found ${other}" )
        }
      }
      val failables    : immutable.Seq[Failable[(StringASCII_Exact3, Unsigned16)]] = seq.map( decodeTuple );
      val firstFailure : Option[Failed[(StringASCII_Exact3, Unsigned16)]]          = failables.find( _.isFailed ).map( _.asFailed );
      firstFailure.fold( succeed( failables.map( _.get ) ) )( failed => refail( failed ) )
    }
  }
  implicit final object Subprotocol_Core_Hello_RLPSerialzing extends RLPSerializing[Subprotocol.Core.Hello] {
    def toElement( rlpSerializable : Subprotocol.Core.Hello ) : RLP.Element = {
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
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Core.Hello] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, p2pVersionE, clientIdE, capabilitiesE, listenPortE, nodeIdE ) => {
          for {
            typeCode     <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            p2pVersion   <- RLP.fromElement[Unsigned16]( p2pVersionE.simplify )
            clientId     <- RLP.fromElement[StringUTF8]( clientIdE.simplify )
            capabilities <- RLP.fromElement[Subprotocol.Core.Hello.Capabilities]( capabilitiesE.simplify )
            listenPort   <- RLP.fromElement[Unsigned16]( listenPortE.simplify )
            nodeId       <- RLP.fromElement[ByteSeqExact64]( nodeIdE.simplify )
          } yield {
            Subprotocol.Core.Hello( typeCode, p2pVersion, clientId, capabilities, listenPort, nodeId )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Core.Hello: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Core_Disconnect extends RLPSerializing[Subprotocol.Core.Disconnect] {
    def toElement( disconnect : Subprotocol.Core.Disconnect ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode, disconnect.reason );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Core.Disconnect] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, reasonE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            reason   <- RLP.fromElement[Unsigned16]( reasonE.simplify )
          } yield {
            Subprotocol.Core.Disconnect( typeCode, reason )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Core.Disconnect: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Core_Ping extends RLPSerializing[Subprotocol.Core.Ping] {
    def toElement( disconnect : Subprotocol.Core.Ping ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Core.Ping] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.Core.Ping( typeCode )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Core.Ping: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Core_Pong extends RLPSerializing[Subprotocol.Core.Pong] {
    def toElement( disconnect : Subprotocol.Core.Pong ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Core.Pong] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.Core.Pong( typeCode )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Core.Pong: ${other}" );
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
        case other => fail( "Unexpected element for Subprotocol.Eth60.Status: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Eth60_NewBlockHashes extends RLPSerializing[Subprotocol.Eth60.NewBlockHashes] {
    def toElement( nbh : Subprotocol.Eth60.NewBlockHashes ) : RLP.Element = {
      RLP.Element.Seq( RLP.toElement( nbh.typeCode ) +: nbh.hashes.map( RLP.toElement( _ ) ) )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Eth60.NewBlockHashes] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, hashEs @ _* ) => {
          RLP.fromElement[Unsigned16]( typeCodeE.simplify ).flatMap{ typeCode =>
            val failables = hashEs.map( hashE => RLP.fromElement[EthHash]( hashE.simplify ) );
            val mbFirstFailure = failables.find( _.isFailed )
            val failableSeq = mbFirstFailure.fold( succeed( failables.map( _.get ) ) )( failable => refail( failable.asFailed ) )
            failableSeq.map( seq => Subprotocol.Eth60.NewBlockHashes( typeCode, immutable.IndexedSeq( seq : _* ) ) )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Eth60.NewBlockHashes: ${other}" );
      }
    }
  }
}
