package com.mchange.sc.v1.consuela.ethereum

import jsonrpc20._

import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.consuela._

import scala.annotation.tailrec

import scala.io.Codec

import scala.collection._

package object ethabi {
  val IdentifierLength = 4

  def identifierForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abiDefinition : Abi.Definition ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndTypes( functionName, functionTypes, abiDefinition ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abiDefinition : Abi.Definition ) : Failable[String] = {
    abiFunctionForFunctionNameAndTypes( functionName, functionTypes, abiDefinition ).map( signatureForAbiFunction )
  }
  def abiFunctionForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abiDefinition : Abi.Definition ) : Failable[Abi.Function] = {
    val candidates = abiFunctionsForFunctionName( functionName, abiDefinition )
    val usable = candidates.filter( checkTypesForFunction( functionTypes, _  ) )
    usable.length match {
      case 0 => fail( s"""No matching function '${functionName}' for types '${functionTypes.mkString(",")}' in ABI: ${abiDefinition}""" )
      case 1 => succeed( usable.head )
      case 2 => fail(
        s"""Ambiguous, multiple matches of function '${functionName} for types '${functionTypes.mkString(",")}' in ABI: ${abiDefinition}""" +
          "\n\t" +
          usable.mkString("\n\t") +
          "\n"
      )
    }
  }
  def callDataForAbiFunction( args : Seq[String], abiFunction : Abi.Function ) : Failable[immutable.Seq[Byte]] = {
    def headTail( enc : Encoder[_], arg : String ) : Failable[Tuple2[Option[immutable.Seq[Byte]],immutable.Seq[Byte]]] = {
      enc.parseEncode( arg ).map( encoded =>
        if ( enc.encodesDynamicType ) Tuple2( None, encoded ) else Tuple2( Some(encoded), Nil )
      )
    }
    def headTailTupled( tup : Tuple2[Encoder[_],String] ) = headTail( tup._1, tup._2 )

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
      encoders <- inputEncodersForAbiFunction( abiFunction )
      signature = signatureForAbiFunction( abiFunction )
      identifier = identifierForSignature( signature )
      headTails <- Failable.sequence( encoders.zip(args).map( headTailTupled ) )
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
  def constructorCallData( args : Seq[String], abi : Abi.Definition ) : Failable[immutable.Seq[Byte]] = {

    def constructorAsFunction( ctor : Abi.Constructor ) : Abi.Function = {
      val inputs = ctor.inputs.map( ci => Abi.Function.Parameter( ci.name, ci.`type` ) )
      Abi.Function( "<bullshit-arbitrary-constructor-name>", inputs, Nil, false, false )
    }

    if ( abi.constructors.isEmpty ) {
      succeed( Nil )
    } else {
      for {
        _                  <- (abi.constructors.length == 1).toFailable(s"The ABI contains multiple constructors, but constructor overloading is not currently supported (or legal in solidity): ${abi.constructors})")
        ctorAsFunction     <- succeed( constructorAsFunction( abi.constructors.head ) )
        asFunctionCallData <- callDataForAbiFunction( args, ctorAsFunction )
      } yield {
        asFunctionCallData.drop( IdentifierLength )
      }
    }
  }

  case class DecodedReturnValue( parameter : Abi.Function.Parameter, value : Any, stringRep : String )

  def decodeReturnValuesForFunction( returnData : immutable.Seq[Byte], abiFunction : Abi.Function ) : Failable[immutable.Seq[DecodedReturnValue]] = {
    outputEncodersForAbiFunction( abiFunction ).flatMap { encoders =>

      @tailrec
      def readValues( nextHeaderOffset : Int, remainingEncoders : Seq[Encoder[_]], remainingParams : Seq[Abi.Function.Parameter], reverseAccum : List[DecodedReturnValue] ) : immutable.Seq[DecodedReturnValue] = {
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
          readValues( nextNextHeaderOffset, remainingEncoders.tail, remainingParams.tail, DecodedReturnValue( nextParam, nextValue, nextStringRep ) :: reverseAccum )
        }
      }

      Failable( readValues( 0, encoders, abiFunction.outputs, Nil ) )
    }
  }
  def identifierForFunctionNameAndArgs( functionName : String, args : Seq[String], abiDefinition : Abi.Definition ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndArgs( functionName, args, abiDefinition ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndArgs( functionName : String, args : Seq[String], abiDefinition : Abi.Definition ) : Failable[String] = {
    abiFunctionForFunctionNameAndArgs( functionName, args, abiDefinition ).map( signatureForAbiFunction )
  }
  def abiFunctionForFunctionNameAndArgs( functionName : String, args : Seq[String], abiDefinition : Abi.Definition ) : Failable[Abi.Function] = {
    val candidates = abiFunctionsForFunctionName( functionName, abiDefinition )
    val usable = candidates.filter( candidate => checkArgsForFunction( args, candidate ) )
    usable.length match {
      case 0 => fail( s"""No matching function '${functionName}' for args '${args.mkString(",")}' in ABI: ${abiDefinition}""" )
      case 1 => succeed( usable.head )
      case 2 => fail(
        s"""Ambiguous, multiple matches of function '${functionName} for args '${args.mkString(",")}' in ABI: ${abiDefinition}""" +
          "\n\t" +
          usable.mkString("\n\t") +
          "\n"
      )
    }
  }
  def abiFunctionsForFunctionName( functionName : String, abiDefinition : Abi.Definition ) : immutable.Seq[Abi.Function] = {
    abiDefinition.functions.filter( _.name == functionName )
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
  private def encodersForAbiFunctionParameters( params : Seq[Abi.Function.Parameter] ) : Failable[immutable.Seq[Encoder[_]]] = {
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
    encodersForAbiFunctionParameters( function.inputs )
  }
  private def outputEncodersForAbiFunction( function : Abi.Function ) : Failable[immutable.Seq[Encoder[_]]] = {
    encodersForAbiFunctionParameters( function.outputs )
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
