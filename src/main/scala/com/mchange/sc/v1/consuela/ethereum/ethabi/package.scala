package com.mchange.sc.v1.consuela.ethereum

import jsonrpc20._

import com.mchange.sc.v2.failable._

import scala.io.Codec

import scala.collection._

import com.mchange.sc.v1.log.MLevel._

package object ethabi {
  private implicit lazy val logger = mlogger( this )

  def identifierForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abiDefinition : Abi.Definition ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndTypes( functionName, functionTypes, abiDefinition ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndTypes( functionName : String, functionTypes : Seq[String], abiDefinition : Abi.Definition ) : Failable[String] = {
    abiFunctionForFunctionNameAndTypes( functionName, functionTypes, abiDefinition ).map( signatureForFunction )
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
  def identifierForFunctionNameAndArgs( functionName : String, args : Seq[String], abiDefinition : Abi.Definition ) : Failable[immutable.Seq[Byte]] = {
    signatureForFunctionNameAndArgs( functionName, args, abiDefinition ).map( identifierForSignature )
  }
  def signatureForFunctionNameAndArgs( functionName : String, args : Seq[String], abiDefinition : Abi.Definition ) : Failable[String] = {
    abiFunctionForFunctionNameAndArgs( functionName, args, abiDefinition ).map( signatureForFunction )
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
  def signatureForFunction( function : Abi.Function ) : String = {
    val sb = new StringBuilder(256) //XXX: hard-coded
    sb.append( function.name )
    sb.append( '(' )
    sb.append( function.inputs.map( f => canonicalizeTypeName( f.`type` ) ).mkString(",") )
    sb.append( ')' )
    sb.toString
  }

  private [ethabi] val TypeAliases : Map[String,String] = Map (
    "byte"   -> "bytes1",
    "uint"   -> "uint32",
    "int"    -> "int32",
    "fixed"  -> "fixed128x128",
    "ufixed" -> "ufixed128x128"
  )
  private [ethabi] def canonicalizeTypeName( rawTypeName : String ) : String = {
    TypeAliases.get( rawTypeName ).getOrElse( rawTypeName.filter( c => !c.isWhitespace ) )
  }

  private def identifierForSignature( signature : String ) : immutable.Seq[Byte] = {
    EthHash.hash( signature.getBytes( Codec.UTF8.charSet ) ).bytes.take(4)
  }
  private def checkArgsForFunction( args : Seq[String], function : Abi.Function ) : Boolean = {
    val len = args.length
    if ( len != function.inputs.length ) {
      false
    } else {
      val encoders = function.inputs.map( param => Encoder.encoderForSolidityType( param.`type` ) )
      encoders.zip( args ).map {
        case ( mbenc, arg ) => mbenc.fold( fail(s"Encoder not found for ${arg}").asInstanceOf[Failable[Any]].xwarn() )( _.parse( arg ) )
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
