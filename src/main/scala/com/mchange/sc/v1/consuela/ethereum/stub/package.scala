package com.mchange.sc.v1.consuela.ethereum

import scala.collection._
import com.mchange.sc.v1.consuela.ethereum.EthereumException

// TODO: Fixed rational types are not yet implemented
//       (Are they implemenetd in solidity?)

package object stub {

  lazy val Zero = sol.UInt256(0) // if not lazy, we have weirdness because the types aren't initialized

  class StubException( message : String, t : Throwable = null ) extends EthereumException( message, t )

  final object ScalaParameterHelper {
    def apply( scalaTypeName : String ) : ScalaParameterHelper = this.apply( scalaTypeName, identity, name => s"${name}.asInstanceOf[${scalaTypeName}]" )
  }
  final case class ScalaParameterHelper( scalaTypeName : String, inConversionGen : String => String, outConversionGen : String => String )

  def anyIntegralToBigInt( a : Any ) : BigInt = {
    a match {
      case b  : Byte   => BigInt( b )
      case s  : Short  => BigInt( s )
      case i  : Int    => BigInt( i )
      case l  : Long   => BigInt( l )
      case bi : BigInt => bi
      case _           => throw new StubException( s"${a} is not an integral type, cannot be converted to BigInt." )
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
