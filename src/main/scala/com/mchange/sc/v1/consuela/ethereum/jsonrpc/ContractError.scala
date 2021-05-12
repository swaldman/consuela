package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.ethabi.{decodeError,Decoded}

import scala.collection._

import com.mchange.sc.v3.failable._
import com.mchange.sc.v3.failable.logging._

import play.api.libs.json._

import com.mchange.sc.v1.log.MLevel._

final object ContractError {
  private implicit lazy val logger = mlogger( this )

  def message( err : Abi.Error, values : immutable.Seq[Decoded.Value], methodDescriptor : Option[String] ) : String = {
    def param( value : Decoded.Value ) : String = s"${value.parameter.name}->${value.stringRep}"
    def params = values.map(param).mkString(", ")
    def paramsPart = if (values.nonEmpty) s", with parameters: ${params}" else ""
    def mdPart = if (methodDescriptor.nonEmpty) s" -- ${methodDescriptor.get}" else ""
    s"The contract reported an error of type ${err.name}${paramsPart}${mdPart}"
  }

  def fromClientException( ce : ClientException, errors : immutable.Seq[Abi.Error] ) : Option[ContractError] = {
    if (errors.nonEmpty && ce.errorData.nonEmpty && ce.decodedRevertMessage.isEmpty && ce.decodedPanicCode.isEmpty) {
      ce.errorData match {
        case Some( jss : JsString ) => {
          val failable = {
            for {
              errorBytes <- toErrorHexBytes( jss.value )
              ( err, values ) <- decodeError( errors, errorBytes )
            }
            yield {
              new ContractError( err, values, ce.methodDescriptor )
            }
          }
          failable.xdebug("Failure converting ClientException to ContractError: ").toOption
        }
        case _ => {
          DEBUG.log(s"Unexpected JSON error data, cannot decode: ${ce.errorData}")
          None
        }
      }
    }
    else {
      None
    }
  }

  def decodeIfPossibleAndThrow( ce : ClientException, errors : immutable.Seq[Abi.Error] ) : Nothing = throw fromClientException( ce, errors ).getOrElse( ce )
}

final class ContractError( err : Abi.Error, values : immutable.Seq[Decoded.Value], methodDescriptor : Option[String], cause : Throwable = null ) extends ConsuelaException( ContractError.message(err, values, methodDescriptor), cause )
