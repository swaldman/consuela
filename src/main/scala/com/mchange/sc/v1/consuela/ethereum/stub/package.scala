package com.mchange.sc.v1.consuela.ethereum

import scala.collection._
import scala.concurrent.duration._
import scala.math.Ordering
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Abi,Client,Invoker}

// TODO: Fixed rational types are not yet implemented
//       (Are they implemenetd in solidity?)

package object stub {

  class StubException( message : String, t : Throwable = null ) extends EthereumException( message, t )

  val  MarkupOrOverride = Invoker.MarkupOrOverride
  type MarkupOrOverride = Invoker.MarkupOrOverride

  val  Markup = Invoker.Markup
  type Markup = Invoker.Markup

  val  Override = Invoker.Override
  type Override = Invoker.Override

  val DefaultEventConfirmations = 12
  val DefaultGasLimitMarkup     = Markup(0.2)
  val DefaultPollPeriod         = 3.seconds
  val DefaultPollTimeout        = Duration.Inf

  final object ScalaParameterHelper {
    def apply( scalaTypeName : String ) : ScalaParameterHelper = this.apply( scalaTypeName, identity, name => s"${name}.asInstanceOf[${scalaTypeName}]" )
  }
  final case class ScalaParameterHelper( scalaTypeName : String, inConversionGen : String => String, outConversionGen : String => String )

  val AbiEventOrdering = {
    import Ordering.Implicits._ // for Seq ordering
    implicit val EventParamOrdering = Ordering.by( (param : Abi.Event.Parameter) => Tuple3( param.name, param.`type`, param.indexed ) )
    Ordering.by( ( event : Abi.Event ) => Tuple3( event.name, event.inputs, event.anonymous ) )
  }

  private [stub] def abiEventToResolvedName( event : Abi.Event, abi : Abi ) : String = {
    val name = event.name
    val allEventsWithName = abi.events.filter( _.name == name )
    if ( allEventsWithName.length == 1 ) {
      name
    }
    else if ( allEventsWithName.length > 1 ) {
      val ordered = immutable.TreeSet.empty[Abi.Event]( AbiEventOrdering ) ++ allEventsWithName
      val pairs = ordered.zip( Stream.from( 0 ) )
      pairs.find( _._1 == event ) match {
        case Some( ( _, index )  ) => {
          event.name + "_" + index
        }
        case other => {
          throw new StubException( s"Huh? Searching for an event apparently not in the ABI! event: ${event}, abi: ${abi}" )
        }
      }
    }
    else {
      throw new StubException( s"Huh? Searching for an event in an ABI without events! event: ${event}, abi: ${abi}, abi.events.length ${abi.events.length}" )
    }
  }

  val StringHelper = {
    ScalaParameterHelper(
      "sol.String",
      name => s"""${name}.getBytes( java.nio.charset.StandardCharsets.UTF_8 ).toImmutableSeq""",
      name => s"""(new String( ${name}.asInstanceOf[immutable.Seq[Byte]].toArray, java.nio.charset.StandardCharsets.UTF_8))""" )
  }

  val FullTypenameMappings = Map (
    "address" -> ScalaParameterHelper( "sol.Address" ),
    "bool"    -> ScalaParameterHelper( "sol.Bool" ),
    "byte"    -> ScalaParameterHelper( "sol.Byte" ),
    "bytes"   -> ScalaParameterHelper( "sol.Bytes" ),
    "string"  -> StringHelper
  )

  val PredefinedBytesTypeRegex = """^bytes(\d{1,2})""".r
  val IntegralTypeRegex        = """^(u)?int(\d{1,3})$""".r
  val ArrayTypeRegex           = """^(.*)\[(\d*)\]$""".r

  def mbPredefinedBytesType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case PredefinedBytesTypeRegex( len ) => {
        val scalaTypeName = s"sol.Bytes${len}"
        Some( ScalaParameterHelper( scalaTypeName, name => s"${name}.widen", name => s"${scalaTypeName}( ${name}.asInstanceOf[immutable.Seq[Byte]] )" ) )
      }
      case _ => None
    }
  }

  def mbIntegralType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case IntegralTypeRegex( mbu, bitlength ) => {
        val scalaTypeName = if (mbu == "u") s"sol.UInt${bitlength}" else s"sol.Int${bitlength}"
        Some( ScalaParameterHelper( scalaTypeName, name => s"anyIntegralToBigInt( ${name}.widen )", name => s"${scalaTypeName}( anyIntegralToBigInt( ${name} ) )" ) )
      }
      case _ => None
    }
  }

  def mbArrayType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case ArrayTypeRegex( baseTypeName,  mblen ) => {
        scalaParameterHelperForSolidityType( baseTypeName ).map { baseTypeHelper =>
          ScalaParameterHelper(
            s"immutable.Seq[${baseTypeHelper.scalaTypeName}]",
            name => s"""com.mchange.sc.v1.consuela.ethereum.ethabi.Encoder.ArrayRep( "${baseTypeName}", ${name}.map( elem => ${baseTypeHelper.inConversionGen("elem")} ) )""",
            name => s"""${name}.items.map( elem => ${baseTypeHelper.outConversionGen("elem")} )"""
          )
        }
      }
      case _ => None
    }
  }

  def scalaParameterHelperForSolidityType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    FullTypenameMappings.get( solidityTypeName ) orElse mbPredefinedBytesType( solidityTypeName ) orElse mbIntegralType( solidityTypeName ) orElse mbArrayType( solidityTypeName )
  }
}
