package com.mchange.sc.v1.consuela.ethereum.ethabi

import scala.collection._

package object stub {

  final object ScalaParameterHelper {
    def apply( solidityTypeName : String ) : ScalaParameterHelper = this.apply( solidityTypeName, identity, identity, identity )
  }
  final case class ScalaParameterHelper( solidityTypeName : String, restrictionGen : String => String, inConversionGen : String => String, outConversionGen : String => String )

  val StubImports = immutable.Seq(
    "scala.collection._",
    "com.mchange.sc.v1.consuela.ethereum.EthAddress",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.stub.Util._"
  )

  val FullTypenameMappings = Map (
    "address" -> ScalaParameterHelper( "EthAddress" ),
    "bool"    -> ScalaParameterHelper( "Boolean" ),
    "byte"    -> ScalaParameterHelper( "Byte" ),
    "bytes"   -> ScalaParameterHelper( "immutable.Seq[Byte]" ),
    "string"  -> ScalaParameterHelper( "String" )
  )

  val IntegralTypeRegex = """(u)?int(\d{1,3})""".r
  val ArrayTypeRegex    = """(.*)[(\d*)]""".r

  def mbIntegralType( solidityTypeName : String ) : Option[ScalaParameterHelper] = {
    solidityTypeName match {
      case IntegralTypeRegex( "u", bitlength ) => {
        val solidityTypeName = "BigInt"
        val restrictionGen = ( v : String ) => s"restrictValidUnsigned( $bitlength )( $v )"
        Some( ScalaParameterHelper( solidityTypeName, restrictionGen, identity, identity ) )
      }
      case IntegralTypeRegex( _, bitlength ) => {
        val solidityTypeName = "BigInt"
        val restrictionGen = ( v : String ) => s"restrictValidSigned( $bitlength )( $v )"
        Some( ScalaParameterHelper( solidityTypeName, restrictionGen, identity, identity ) )
      }
      case _ => None
    }
  }

  def scalaParameterHelperForSolidityType( solidityTypeName : String ) : String = ???

}
