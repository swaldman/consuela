package com.mchange.sc.v1.consuela.ethereum.ethabi.stub

import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.Abi

import scala.collection._

import java.io.StringWriter

import com.mchange.sc.v2.lang.borrow

import com.mchange.v2.io.IndentedWriter

object Generator {

  private val StubImports = immutable.Seq(
    "scala.collection._",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.stub._"
  )

  def generateStub( className : String, abi : Abi.Definition, fullyQualifiedPackageName : String ) : String = {
    val sw = new StringWriter()

    borrow( new IndentedWriter( sw ) ) { iw =>
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()

      iw.println( s"final case class $className( address : EthAddress ) {" )
      iw.upIndent()
      iw.println( s"final object transaction {" )
      iw.upIndent()
      ???
      iw.downIndent()
      iw.println( "}" )
      iw.println( s"final object constant {" )
      iw.upIndent()
      ???
      iw.downIndent()
      iw.println( "}" )
      iw.downIndent()
      iw.println( "}" )
    }

    sw.toString
  }

  private def forceHelper( solidityTypeName : String ) : ScalaParameterHelper = {
    scalaParameterHelperForSolidityType( solidityTypeName ).getOrElse {
      throw new StubException( s"No Scala type information available for solidity type '$solidityTypeName'." )
    }
  }

  private def functionSignature( fcn : Abi.Function, constantSection : Boolean ) : String = {
    def param( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      s"${param.name} : ${helper.scalaTypeName}"
    }

    def scalaType( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      s"${helper.scalaTypeName}"
    }
    
    val prereturn = s"""def ${fcn.name}( ${fcn.inputs.map(param).mkString(", ")} )( optionalValueInWei : Option[sol.UInt256] )( implicit context : InvocationContext )"""

    val post = {
      if (!constantSection) {
        " : EthHash"
      } else {
        fcn.outputs.length match {
          case 1 => {
            s" : ${scalaType( fcn.outputs.head )}"
          }
          case _ => {
            s""" : ( ${fcn.outputs.map( scalaType ).mkString(", ")} )"""
          }
        }
      }
    }

    prereturn + post
  }
}
