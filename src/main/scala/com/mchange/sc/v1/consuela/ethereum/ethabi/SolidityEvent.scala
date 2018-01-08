package com.mchange.sc.v1.consuela.ethereum.ethabi

import com.mchange.sc.v1.consuela.ethereum.{jsonrpc,EthAddress,EthHash,EthLogEntry}
import jsonrpc.Abi

import com.mchange.sc.v2.failable._

import scala.collection._

import scala.io.Codec

final object SolidityEvent {
  def computeIdentifierTopic( event : Abi.Event ) : EthLogEntry.Topic = {
    val eventSignature = event.name + "(" + event.inputs.map( param => canonicalizeTypeName( param.`type` ) ).mkString(",") + ")"
    EthLogEntry.Topic( EthHash.hash( eventSignature.getBytes( Codec.UTF8.charSet ) ).bytes )
  }
  case class Interpretor( abi : Abi ) {
    private lazy val identifiers: immutable.Map[EthLogEntry.Topic,Abi.Event] = {
      abi.events.map( event => ( computeIdentifierTopic( event ), event ) ).toMap
    }

    // def decodeOutValues( params : immutableSeq[Abi.Parameters], f_encoders : Failable[immutable.Seq[Encoder[_]]] )( returnData : immutable.Seq[Byte] ) : Failable[immutable.Seq[Decoded.Value]] = ???
    // case class Decoded.Value( parameter : Abi.Parameter, value : Any, stringRep : String )

    def interpret( log : EthLogEntry ) : Failable[SolidityEvent] = {
      if ( log.topics.nonEmpty ) {
        identifiers.get( log.topics(0) ) match {
          case None             => succeed( Anonymous( log ) )
          case Some( abiEvent ) => interpretLogEntryAsEvent( log, abiEvent ) map { allParams =>
            Named( allParams, log, abiEvent )
          }
        }
      }
      else {
        succeed( Anonymous( log ) )
      }
    }
  }

  final case class Named( inputs : immutable.Seq[Decoded.Value], logEntry : EthLogEntry, abiEvent : Abi.Event ) extends SolidityEvent {
    def name = abiEvent.name
    def address = logEntry.address

    def signatureTopic : EthLogEntry.Topic = logEntry.topics(0)
  }

  final case class Anonymous( logEntry : EthLogEntry ) extends SolidityEvent {
    def address = logEntry.address
    def interpretAs( abiEvent : Abi.Event ) : Failable[immutable.Seq[Decoded.Value]] = {
      if ( abiEvent.anonymous ) {
        interpretLogEntryAsEvent( logEntry, abiEvent )
      }
      else {
        fail( "Cannot interpret SolidityEvent.Anonymous '${this}' as named event '${abiEvent}'" )
      }
    }
  }
}
trait SolidityEvent {
  def address  : EthAddress
}
