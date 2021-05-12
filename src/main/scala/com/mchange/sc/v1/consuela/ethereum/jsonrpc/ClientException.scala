package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.ethabi.{decodeFunctionCall,Decoded}
import com.mchange.sc.v3.failable._

import com.mchange.sc.v2.jsonrpc.{Response => JsonrpcResponse}

import scala.collection._

import play.api.libs.json._

final object ClientException {

  private final val ShadowAbi = {
    Abi("""[{"name":"Error","inputs":[{"name":"message","type":"string"}],"outputs":[],"type":"function","stateMutability":"pure"},{"name":"Panic","inputs":[{"name":"code","type":"uint256"}],"outputs":[],"type":"function","stateMutability":"pure"}]""")
  }

  def apply( methodDescriptor : String, report : JsonrpcResponse.Error.Report ) : ClientException = {
    val errorCode    = Some( report.code )
    val errorMessage = Some( report.message )
    val errorData    = report.data

    val ( errorDataParseFailure, decodedRevertMessage, decodedPanicCode ) = {

      def extractRevertInfo( fcn : Abi.Function, values : immutable.Seq[Decoded.Value] ) : Failable[Either[String,BigInt]] = Failable.flatCreate {
        if (fcn.name == "Error") {
          val messages = values.filter( _.parameter.name == "message" )
          require( messages.size == 1, s"Expected precisely one 'message' parameter, found ${messages.size}: " + messages.mkString(",") )
          Failable.succeed( Left(messages.head.stringRep) )
        }
        else if (fcn.name == "Panic") {
          val codes = values.filter( _.parameter.name == "code" )
          require( codes.size == 1, s"Expected precisely one 'code' parameter, found ${codes.size}: " + codes.mkString(",") )
          Failable.succeed( Right(codes.head.value.asInstanceOf[BigInt]) )
        }
        else {
          Failable.fail(s"Decoded unexpected function from errorData: ${fcn}", includeStackTrace=false)
        }
      }

      errorData match {
        case Some( jss : JsString ) => {
          val f_either = {
            for {
              hexBytes        <- toErrorHexBytes( jss.value )
              ( fcn, values ) <- decodeFunctionCall( ShadowAbi, hexBytes )
              either          <- extractRevertInfo( fcn, values )
            }
            yield {
              either
            }
          }
          f_either match {
            case Succeeded( Left(message) ) => ( None, Some( message ), None )
            case Succeeded( Right(code) )   => ( None, None, Some(code) )
            case Failed( source )     => ( Some( source.toString ), None, None )
          }
        }
        case Some( other ) => {
          ( Some(s"Expected JSON String as error data, found: ${other}"), None, None )
        }
        case None => {
          ( None, None, None )
        }
      }
    }
    def optionalItem[T]( name : String, item : Option[T] ) = item.map(i => name + "=" + i)
    val basePart = errorMessage.getOrElse("(No error message)")
    val items = {
      optionalItem("decodedRevertMessage", decodedRevertMessage) ::
      optionalItem("decodedPanicCode", decodedPanicCode.map(c=>"0x"+c.toString(16))) ::
      optionalItem("errorCode", errorCode) ::
      optionalItem("rawErrorData", errorData) ::
      optionalItem("errorDataParseFailure", errorDataParseFailure) ::
      Nil
    }
    val itemsPart = " -- " + items.collect{ case Some(s) => s }.mkString("; ")
    val message = basePart + itemsPart + s"; ${methodDescriptor}"

    new ClientException( errorCode, errorMessage, errorData, decodedRevertMessage, decodedPanicCode, message )
  }
}
final class ClientException private (
  val errorCode : Option[Int] = None,
  val errorMessage : Option[String] = None,
  val errorData : Option[JsValue] = None,
  val decodedRevertMessage : Option[String],
  val decodedPanicCode : Option[BigInt],
  val message : String,
  val cause : Throwable = null
) extends ConsuelaException( message, cause )
