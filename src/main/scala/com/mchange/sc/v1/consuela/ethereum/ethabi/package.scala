package com.mchange.sc.v1.consuela.ethereum

import jsonrpc._

import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.consuela._

import scala.annotation.tailrec

import scala.io.Codec

import scala.collection._

import EthLogEntry.Topic

import com.mchange.sc.v1.log.MLevel._

package object ethabi {
  implicit lazy val logger = mlogger( this )

  val IdentifierLength = 4

  def identifierForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abi : Abi ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndTypes( functionName, functionTypes, abi ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abi : Abi ) : Failable[String] = {
    abiFunctionForFunctionNameAndTypes( functionName, functionTypes, abi ).map( signatureForAbiFunction )
  }
  def abiFunctionForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abi : Abi ) : Failable[Abi.Function] = {
    val candidates = abiFunctionsForFunctionName( functionName, abi )
    val usable = candidates.filter( checkTypesForFunction( functionTypes, _  ) )
    usable.length match {
      case 0 => fail( s"""No matching function '${functionName}' for types '${functionTypes.mkString(",")}' in ABI: ${abi}""" )
      case 1 => succeed( usable.head )
      case 2 => fail(
        s"""Ambiguous, multiple matches of function '${functionName} for types '${functionTypes.mkString(",")}' in ABI: ${abi}""" +
          "\n\t" +
          usable.mkString("\n\t") +
          "\n"
      )
    }
  }

  final object SolidityEvent {
    case class Interpretor( abi : Abi ) {
      private lazy val identifiers: immutable.Map[EthLogEntry.Topic,Abi.Event] = {
        def computeTopic( event : Abi.Event ) : EthLogEntry.Topic = {
          val eventSignature = event.name + "(" + event.inputs.map( param => canonicalizeTypeName( param.`type` ) ).mkString(",") + ")"
          Topic( EthHash.hash( eventSignature.getBytes( Codec.UTF8.charSet ) ).bytes )
        }
        abi.events.map( event => ( computeTopic( event ), event ) ).toMap
      }
      private def indexedNonindexed( event : Abi.Event ) : ( immutable.Seq[Abi.Event.Parameter], immutable.Seq[Abi.Event.Parameter] ) = event.inputs.partition( _.indexed )

      // def decodeOutValues( params : immutableSeq[Abi.Parameters], f_encoders : Failable[immutable.Seq[Encoder[_]]] )( returnData : immutable.Seq[Byte] ) : Failable[immutable.Seq[DecodedValue]] = ???
      // case class DecodedValue( parameter : Abi.Parameter, value : Any, stringRep : String )

      private def decodeIndexed( params : immutable.Seq[Abi.Parameter], encoders : immutable.Seq[Encoder[_]], topics : immutable.Seq[EthLogEntry.Topic] ) : Failable[immutable.Seq[DecodedValue]] = {
        Failable.sequence {
          params.zip(encoders).zip( topics ).map { case ( ( param, encoder ), topic ) =>
            for {
              ( decoded, extra ) <- encoder.decode( topic.widen )
              formatted <- encoder.formatUntyped( decoded )
            }
            yield {
              if (extra.nonEmpty ) {
                WARNING.log( s"An event topic is shorter than expected. [param -> $param, topic -> $topic, extra -> ${extra.hex}" )
              }
              DecodedValue( param, decoded, formatted )
            }
          }
        }
      }

      def interpret( log : EthLogEntry ) : Failable[SolidityEvent] = {
        if ( log.topics.nonEmpty ) {
          identifiers.get( log.topics(0) ) match {
            case None => succeed( Anonymous( log ) )
            case Some( abiEvent ) => {
              val (indexed, nonIndexed ) = indexedNonindexed( abiEvent )
              for {
                iencoders  <- encodersForAbiParameters( indexed ) 
                ivalues    <- decodeIndexed( indexed, iencoders, log.topics.tail ) // remember, the head (zeroeth) topic was the identifier
                nivalues   <- decodeOutValues( nonIndexed, encodersForAbiParameters( nonIndexed ) )( log.data )
              }
              yield {
                val pmap = (ivalues ++ nivalues).map( dov => ( dov.parameter, dov ) ).toMap
                val allParams = abiEvent.inputs.map( pmap )
                Named( log.address, abiEvent.name, allParams )
              }
            }
          }
        }
        else {
          succeed( Anonymous( log ) )
        }
      }
    }

    final case class Named( address : EthAddress, name : String, inputs : immutable.Seq[DecodedValue] ) extends SolidityEvent

    object Anonymous {
      def apply( log : EthLogEntry ) = new Anonymous( log.address, log.topics, log.data )
    }
    final case class Anonymous( address : EthAddress, topics : immutable.Seq[EthLogEntry.Topic], data : immutable.Seq[Byte] ) extends SolidityEvent
  }
  trait SolidityEvent {
    def address : EthAddress
  }

  def callDataForAbiFunctionFromEncoderRepresentations( reps : Seq[Any], abiFunction : Abi.Function ) : Failable[immutable.Seq[Byte]] = {
    def headTail( enc : Encoder[_], rep : Any ) : Failable[Tuple2[Option[immutable.Seq[Byte]],immutable.Seq[Byte]]] = {
      enc.encodeUntyped( rep ).map( encoded =>
        if ( enc.encodesDynamicType ) Tuple2( None, encoded ) else Tuple2( Some(encoded), Nil )
      )
    }
    def headTailTupled( tup : Tuple2[Encoder[_], Any] ) = headTail( tup._1, tup._2 )

    val fEncoders  = inputEncodersForAbiFunction( abiFunction )
    val fHeadTails = fEncoders.flatMap( encoders => Failable.sequence( encoders.zip(reps).map( headTailTupled ) ) )

    _callDataForAbiFunction( fEncoders, fHeadTails, abiFunction )
  }

  def callDataForAbiFunctionFromStringArgs( args : Seq[String], abiFunction : Abi.Function ) : Failable[immutable.Seq[Byte]] = {
    def headTail( enc : Encoder[_], arg : String ) : Failable[Tuple2[Option[immutable.Seq[Byte]],immutable.Seq[Byte]]] = {
      enc.parseEncode( arg ).map( encoded =>
        if ( enc.encodesDynamicType ) Tuple2( None, encoded ) else Tuple2( Some(encoded), Nil )
      )
    }
    def headTailTupled( tup : Tuple2[Encoder[_],String] ) = headTail( tup._1, tup._2 )

    val fEncoders  = inputEncodersForAbiFunction( abiFunction )
    val fHeadTails = fEncoders.flatMap( encoders => Failable.sequence( encoders.zip(args).map( headTailTupled ) ) )

    _callDataForAbiFunction( fEncoders, fHeadTails, abiFunction )
  }

  private def _callDataForAbiFunction(
    fEncoders : Failable[immutable.Seq[Encoder[_]]],
    fHeadTails : Failable[immutable.Seq[Tuple2[Option[immutable.Seq[Byte]],immutable.Seq[Byte]]]],
    abiFunction : Abi.Function
  ) : Failable[immutable.Seq[Byte]] = {

    def generateCallData( identifier : immutable.Seq[Byte], headTails : Seq[Tuple2[Option[immutable.Seq[Byte]],immutable.Seq[Byte]]] ) : Failable[immutable.Seq[Byte]] = Failable {
      val totalHeadSize = headTails.foldLeft(0)( ( count, next ) => count + next._1.fold( Encoder.DynamicHeadSize )( _.length ) )

      // XXX: hard-coded initial buffer size
      // XXX: should i bother redoing/making this in functional style?
      val buffer = new mutable.ArrayBuffer[Byte](1024)
      buffer ++= identifier

      var lastDynamicOffset = totalHeadSize

      headTails.foreach { ht =>
        if ( ht._1.isEmpty ) {
          buffer ++= Encoder.UInt256.encode( BigInt( lastDynamicOffset ) ).get // Failable constructor will catch and convert to failure
          lastDynamicOffset += ht._2.length
        } else {
          buffer ++= ht._1.get
        }
      }
      headTails.filter( _._1.isEmpty ).foreach { ht =>
        buffer ++= ht._2
      }
      buffer.toArray.toImmutableSeq
    }

    for {
      encoders <- fEncoders
      headTails <- fHeadTails
      signature = signatureForAbiFunction( abiFunction )
      identifier = identifierForSignature( signature )
      callData <- generateCallData( identifier, headTails )
    } yield {
      callData
    }
  }

  /*
   * bit ugly, but rather than refactor callDataForAbiFunction(...)
   * - we generate a fake "function" as the constructor,
   * - use callDataForAbiFunction(...)
   * - then drop the meaningles identifier
   */ 
  def constructorCallData( args : Seq[String], abi : Abi ) : Failable[immutable.Seq[Byte]] = {

    def constructorAsFunction( ctor : Abi.Constructor ) : Abi.Function = {
      val inputs = ctor.inputs.map( ci => Abi.Function.Parameter( ci.name, ci.`type` ) )
      Abi.Function( "<bullshit-arbitrary-constructor-name>", inputs, Nil, false, false, "nonpayable" )
    }

    if ( abi.constructors.isEmpty ) {
      succeed( Nil )
    } else {
      for {
        _                  <- (abi.constructors.length == 1).toFailable(s"The ABI contains multiple constructors, but constructor overloading is not currently supported (or legal in solidity): ${abi.constructors})")
        ctorAsFunction     <- succeed( constructorAsFunction( abi.constructors.head ) )
        asFunctionCallData <- callDataForAbiFunctionFromStringArgs( args, ctorAsFunction )
      } yield {
        asFunctionCallData.drop( IdentifierLength )
      }
    }
  }

  case class DecodedValue( parameter : Abi.Parameter, value : Any, stringRep : String )

  def decodeReturnValuesForFunction( returnData : immutable.Seq[Byte], abiFunction : Abi.Function ) : Failable[immutable.Seq[DecodedValue]] = {
    decodeOutValues( abiFunction.outputs, outputEncodersForAbiFunction( abiFunction ) )( returnData )
  }
  def decodeOutValues( params : immutable.Seq[Abi.Parameter], f_encoders : Failable[immutable.Seq[Encoder[_]]] )( returnData : immutable.Seq[Byte] ) : Failable[immutable.Seq[DecodedValue]] = {
    f_encoders.flatMap { encoders =>

      @tailrec
      def readValues( nextHeaderOffset : Int, remainingEncoders : Seq[Encoder[_]], remainingParams : Seq[Abi.Parameter], reverseAccum : List[DecodedValue] ) : immutable.Seq[DecodedValue] = {
        // println( s"nextHeaderOffset : $nextHeaderOffset, remainingEncoders : $remainingEncoders, remainingParams : $remainingParams, reverseAccum : $reverseAccum" )
        if ( remainingEncoders.isEmpty ) {
          reverseAccum.reverse
        } else {
          val nextEncoder = remainingEncoders.head
          val nextParam = remainingParams.head
          val ( nextValue, nextNextHeaderOffset ) = {
            if ( nextEncoder.encodesDynamicType ) {
              val ( offset, _ ) = Encoder.UInt256.decode( returnData.drop( nextHeaderOffset ) ).get // we assert the success of everything here, deal with any Exception in the Failable wrapper
              if ( !offset.isValidInt ) {
                throw new EthereumException( s"${offset} exceeds ${Integer.MAX_VALUE}, not currently supported." )
              }
              val ( value, _ ) = nextEncoder.decode( returnData.drop( offset.toInt ) ).get
              ( value, nextHeaderOffset + Encoder.DynamicHeadSize )
            } else {
              val ( value, _ ) = nextEncoder.decode( returnData.drop( nextHeaderOffset ) ).get
              ( value, nextHeaderOffset + nextEncoder.encodingLength.get )
            }
          }
          val nextStringRep = nextEncoder.formatUntyped( nextValue ).get
          readValues( nextNextHeaderOffset, remainingEncoders.tail, remainingParams.tail, DecodedValue( nextParam, nextValue, nextStringRep ) :: reverseAccum )
        }
      }

      Failable( readValues( 0, encoders, params, Nil ) )
    }
  }
  def identifierForFunctionNameAndArgs( functionName : String, args : Seq[String], abi : Abi ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndArgs( functionName, args, abi ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndArgs( functionName : String, args : Seq[String], abi : Abi ) : Failable[String] = {
    abiFunctionForFunctionNameAndArgs( functionName, args, abi ).map( signatureForAbiFunction )
  }
  def abiFunctionForFunctionNameAndArgs( functionName : String, args : Seq[String], abi : Abi ) : Failable[Abi.Function] = {
    val candidates = abiFunctionsForFunctionName( functionName, abi )
    val usable = candidates.filter( candidate => checkArgsForFunction( args, candidate ) )
    usable.length match {
      case 0 => fail( s"""No matching function '${functionName}' for args '${args.mkString(",")}' in ABI: ${abi}""" )
      case 1 => succeed( usable.head )
      case 2 => fail(
        s"""Ambiguous, multiple matches of function '${functionName} for args '${args.mkString(",")}' in ABI: ${abi}""" +
          "\n\t" +
          usable.mkString("\n\t") +
          "\n"
      )
    }
  }
  def abiFunctionsForFunctionName( functionName : String, abi : Abi ) : immutable.Seq[Abi.Function] = {
    abi.functions.filter( _.name == functionName )
  }
  def signatureForAbiFunction( function : Abi.Function ) : String = {
    val sb = new StringBuilder(256) //XXX: hard-coded
    sb.append( function.name )
    sb.append( '(' )
    sb.append( function.inputs.map( f => canonicalizeTypeName( f.`type` ) ).mkString(",") )
    sb.append( ')' )
    sb.toString
  }

  private [ethabi] val TypeAliases : Map[String,String] = Map (
    "uint"   -> "uint32",
    "int"    -> "int32",
    "fixed"  -> "fixed128x128",
    "ufixed" -> "ufixed128x128"
  )
  private [ethabi] def canonicalizeTypeName( rawTypeName : String ) : String = {
    TypeAliases.get( rawTypeName ).getOrElse( rawTypeName.filter( c => !c.isWhitespace ) )
  }

  private def identifierForSignature( signature : String ) : immutable.Seq[Byte] = {
    EthHash.hash( signature.getBytes( Codec.UTF8.charSet ) ).bytes.take( IdentifierLength )
  }
  private def encodersForAbiParameters( params : Seq[Abi.Parameter] ) : Failable[immutable.Seq[Encoder[_]]] = {
    val mbEncodersTypes = params.map( param => Tuple2(Encoder.encoderForSolidityType( param.`type` ), param.`type` ) )
    val reverseEncoders = mbEncodersTypes.foldLeft( succeed( Nil : List[Encoder[_]] ) ) { ( accum, tup ) =>
      tup match {
        case Tuple2( Some( encoder ), _ ) => accum.map( list => encoder :: list )
        case Tuple2( _, t)                => fail(s"Encoder not found for type ${t}")
      }
    }
    reverseEncoders.map( _.reverse )
  }
  private def inputEncodersForAbiFunction( function : Abi.Function ) : Failable[immutable.Seq[Encoder[_]]] = {
    encodersForAbiParameters( function.inputs )
  }
  private def outputEncodersForAbiFunction( function : Abi.Function ) : Failable[immutable.Seq[Encoder[_]]] = {
    encodersForAbiParameters( function.outputs )
  }
  private def checkArgsForFunction( args : Seq[String], function : Abi.Function ) : Boolean = {
    val len = args.length
    if ( len != function.inputs.length ) {
      false
    } else {
      val mbEncoders = function.inputs.map( param => Encoder.encoderForSolidityType( param.`type` ) )
      mbEncoders.zip( args ).map {
        case ( mbenc, arg ) => mbenc.fold( fail(s"Encoder not found for ${arg}").asInstanceOf[Failable[Any]] )( _.parse( arg ) )
      }.forall( _.isSucceeded )
    }
  }
  private def checkTypesForFunction( desiredTypes : Seq[String], function : Abi.Function ) : Boolean = {
    val len = desiredTypes.length
    if ( len != function.inputs.length ) {
      false
    } else {
      val canonicalDesired  = desiredTypes.map( canonicalizeTypeName )
      val canonicalRequired = function.inputs.map( f => canonicalizeTypeName( f.`type` ) )
      canonicalDesired.zip( canonicalRequired ).forall { case ( des, req ) => des == req }
    }
  }
}
