package com.mchange.sc.v1.consuela.ethereum.ethabi

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{jsonrpc,EthAddress,EthHash,EthLogEntry}
import jsonrpc.Abi

import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.log.MLevel._

import scala.collection._

import scala.io.Codec

final object SolidityEvent {
  private def decodeIndexed( params : immutable.Seq[Abi.Parameter], encoders : immutable.Seq[Encoder[_]], topics : immutable.Seq[EthLogEntry.Topic] ) : Failable[immutable.Seq[Decoded]] = {
    def decode( param : Abi.Parameter, encoder : Encoder[_], topic : EthLogEntry.Topic ) : Failable[Decoded] = {
      if ( encoder.encodesDynamicType ) {
        succeed( Decoded.Hash( param, EthHash.withBytes( topic.widen ) ) )
      }
      else {
        for {
          ( decoded, extra ) <- encoder.decode( topic.widen )
          formatted <- encoder.formatUntyped( decoded )
        }
        yield {
          if (extra.nonEmpty ) {
            WARNING.log( s"An event topic is shorter than expected. [param -> $param, topic -> $topic, extra -> ${extra.hex}" )
          }
          Decoded.Value( param, decoded, formatted )
        }
      }
    }

    val len = params.length

    assert( len == encoders.length, s"""Internal error, we should always have retrieved as many encoders as there ar params, params: ${params.mkString(", ")}, encoders: ${encoders.mkString(", ")}""" )
    if ( len != topics.length ) {
      fail( s"""The number of decodable topics should but does not match the number of indexed params, indexed params: ${params.mkString(", ")}, topics: ${topics.mkString(", ")}""" )
    }
    else {
      Failable.sequence {
        params.zip(encoders).zip( topics ).map { case ( ( param, encoder ), topic ) =>
          decode( param, encoder, topic )
        }
      }
    }
  }

  private def indexedNonindexed( event : Abi.Event ) : ( immutable.Seq[Abi.Event.Parameter], immutable.Seq[Abi.Event.Parameter] ) = event.inputs.partition( _.indexed )

  private def decodableTopics( logEntry : EthLogEntry, abiEvent : Abi.Event ) : Failable[immutable.Seq[EthLogEntry.Topic]] = {
    if ( abiEvent.anonymous ) {
      succeed( logEntry.topics )
    }
    else if ( logEntry.topics.isEmpty ) {
      fail (
        s"A nonanonymous event should contain at least one topic, the event signature. The provided logEntry contains none. [logEntry -> ${logEntry}, abiEvent -> ${abiEvent}]"
      )
    }
    else {
      succeed( logEntry.topics.tail )
    }
  }

  def interpretLogEntryAsEvent( logEntry : EthLogEntry, abiEvent : Abi.Event ) : Failable[immutable.Seq[Decoded]] = {
    val ( indexed, nonIndexed ) = indexedNonindexed( abiEvent )

    // XXX: UGLY, TEMPORARY -- until we fix the failable library to catch Exceptions and yield failures in map / flatMap
    val nestedFailable = {
      for {
        dts        <- decodableTopics( logEntry, abiEvent )
        iencoders  <- encodersForAbiParameters( indexed )
        ivalues    <- decodeIndexed( indexed, iencoders, dts )
        nivalues   <- decodeOutValues( nonIndexed, encodersForAbiParameters( nonIndexed ) )( logEntry.data )
      }
      yield {
        Failable {
          val pmap = (ivalues ++ nivalues).map( d => ( d.parameter, d ) ).toMap
          abiEvent.inputs.map( pmap )
        }
      }
    }
    nestedFailable.flatten
  }


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

  final case class Named( inputs : immutable.Seq[Decoded], logEntry : EthLogEntry, abiEvent : Abi.Event ) extends SolidityEvent {
    def name = abiEvent.name
    def address = logEntry.address

    def signatureTopic : EthLogEntry.Topic = logEntry.topics(0)
  }

  final case class Anonymous( logEntry : EthLogEntry ) extends SolidityEvent {
    def address = logEntry.address
    def interpretAs( abiEvent : Abi.Event ) : Failable[immutable.Seq[Decoded]] = {
      if ( abiEvent.anonymous ) {
        interpretLogEntryAsEvent( logEntry, abiEvent )
      }
      else {
        fail( "Cannot interpret SolidityEvent.Anonymous '${this}' as event '${abiEvent}'" )
      }
    }
  }
}
trait SolidityEvent {
  def address  : EthAddress
}
