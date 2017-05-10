package com.mchange.sc.v1.consuela.ethereum.ethabi

import scala.collection._
import com.mchange.sc.v1.consuela.ethereum.EthereumException

// TODO: Fixed rational types are not yet implemented
//       (Are they implemenetd in solidity?)

package object stub {

  // annoying, but we want a uniform way to convert all integral types,
  // to BigInt, even when we don't know that the type is anything but integral
  implicit final class BigIntBigInter( val bi : BigInt ) extends AnyVal {
    def toBigInt : BigInt = bi
  }

  lazy val Zero = sol.UInt256(0) // if not lazy, we have weirdness because the types aren't initialized

  class StubException( message : String, t : Throwable = null ) extends EthereumException( message, t )

  final object ScalaParameterHelper {
    def apply( solidityTypeName : String ) : ScalaParameterHelper = this.apply( solidityTypeName, identity, identity )
  }
  final case class ScalaParameterHelper( scalaTypeName : String, inConversionGen : String => String, outConversionGen : String => String )

  val FullTypenameMappings = Map (
    "address" -> ScalaParameterHelper( "sol.Address" ),
    "bool"    -> ScalaParameterHelper( "sol.Bool" ),
    "byte"    -> ScalaParameterHelper( "sol.Byte" ),
    "bytes"   -> ScalaParameterHelper( "sol.Bytes" ),
    "string"  -> ScalaParameterHelper( "sol.String" )
  )

  val PredefinedBytesTypeRegex = """^bytes(\d{1,2})""".r
  val IntegralTypeRegex        = """^(u)?int(\d{1,3})$""".r
  val ArrayTypeRegex           = """^(.*)\[(\d*)\]$""".r

  def mbPredefinedBytesType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case PredefinedBytesTypeRegex( len ) => {
        val scalaTypeName = s"sol.Bytes${len}"
        Some( ScalaParameterHelper( scalaTypeName, name => s"${name}.widen", name => s"${scalaTypeName}( $name )" ) )
      }
      case _ => None
    }
  }

  def mbIntegralType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case IntegralTypeRegex( mbu, bitlength ) => {
        val scalaTypeName = if (mbu == "u") s"sol.UInt${bitlength}" else s"sol.Int${bitlength}"
        Some( ScalaParameterHelper( scalaTypeName, name => s"${name}.widen.toBigInt", name => s"${solidityTypeName}( $name )" ) )
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
