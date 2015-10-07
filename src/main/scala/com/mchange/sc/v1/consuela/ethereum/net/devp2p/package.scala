package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.failable._

import java.nio.charset.{Charset,StandardCharsets}
import scala.collection.immutable;

import RLPSerializing.asElement;

package object devp2p {
  implicit final object Subprotocol_Base_Hello_Capabilities extends RLPSerializing[Subprotocol.Base.Hello.Capabilities] {
    def toElement( rlpSerializable : Subprotocol.Base.Hello.Capabilities ) : RLP.Element = {
      RLP.Element.Seq( rlpSerializable.elements.toSeq.map( pair => RLP.Element.Seq.of( RLP.toElement(pair._1), RLP.toElement(pair._2) ) ) )
    }
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Base.Hello.Capabilities] = {
      element match {
        case RLP.Element.Seq( seq ) => decodeTuples( seq ).map( tupleSeq => Subprotocol.Base.Hello.Capabilities( immutable.Set( tupleSeq : _* ) ) )
        case other                  => fail( s"Subprotocol.Base.Hello.Capabilities should be a sequence, found ${other}" )
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
          case other => fail( s"Individual elements of Subprotocol.Base.Hello.Capabilities should be a pair encoded as a sequence, found ${other}" )
        }
      }
      val failables    : immutable.Seq[Failable[(StringASCII_Exact3, Unsigned16)]] = seq.map( decodeTuple );
      val firstFailure : Option[Failed[(StringASCII_Exact3, Unsigned16)]]          = failables.find( _.isFailed ).map( _.asFailed );
      firstFailure.fold( succeed( failables.map( _.get ) ) )( failed => refail( failed ) )
    }
  }
  implicit final object Subprotocol_Base_Hello_RLPSerialzing extends RLPSerializing[Subprotocol.Base.Hello] {
    def toElement( rlpSerializable : Subprotocol.Base.Hello ) : RLP.Element = {
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
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Base.Hello] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, p2pVersionE, clientIdE, capabilitiesE, listenPortE, nodeIdE ) => {
          for {
            typeCode     <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            p2pVersion   <- RLP.fromElement[Unsigned16]( p2pVersionE.simplify )
            clientId     <- RLP.fromElement[StringUTF8]( clientIdE.simplify )
            capabilities <- RLP.fromElement[Subprotocol.Base.Hello.Capabilities]( capabilitiesE.simplify )
            listenPort   <- RLP.fromElement[Unsigned16]( listenPortE.simplify )
            nodeId       <- RLP.fromElement[ByteSeqExact64]( nodeIdE.simplify )
          } yield {
            Subprotocol.Base.Hello( typeCode, p2pVersion, clientId, capabilities, listenPort, nodeId )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Base.Hello: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Base_Disconnect extends RLPSerializing[Subprotocol.Base.Disconnect] {
    def toElement( disconnect : Subprotocol.Base.Disconnect ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode, disconnect.reason );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Base.Disconnect] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE, reasonE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
            reason   <- RLP.fromElement[Unsigned16]( reasonE.simplify )
          } yield {
            Subprotocol.Base.Disconnect( typeCode, reason )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Base.Disconnect: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Base_Ping extends RLPSerializing[Subprotocol.Base.Ping] {
    def toElement( disconnect : Subprotocol.Base.Ping ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Base.Ping] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.Base.Ping( typeCode )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Base.Ping: ${other}" );
      }
    }
  }
  implicit final object Subprotocol_Base_Pong extends RLPSerializing[Subprotocol.Base.Pong] {
    def toElement( disconnect : Subprotocol.Base.Pong ) : RLP.Element = RLP.Element.Seq.of( disconnect.typeCode );
    def fromElement( element : RLP.Element.Basic ) : Failable[Subprotocol.Base.Pong] = {
      element match {
        case RLP.Element.Seq.of( typeCodeE ) => {
          for {
            typeCode <- RLP.fromElement[Unsigned16]( typeCodeE.simplify )
          } yield {
            Subprotocol.Base.Pong( typeCode )
          }
        }
        case other => fail( "Unexpected element for Subprotocol.Base.Pong: ${other}" );
      }
    }
  }
}
