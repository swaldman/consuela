package com.mchange.sc.v1.consuela.ethereum

import play.api.libs.json._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.ethabi.{decodeFunctionCall,Decoded}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}
import com.mchange.sc.v2.playjson._
import com.mchange.sc.v3.failable._

import com.mchange.sc.v2.jsonrpc.{Response => JsonrpcResponse}

import scala.collection._
import scala.util.control.NonFatal

package object jsonrpc {

  // DON'T define JsonrpcException here, it would shadow the Exception of the same name in JsonRpcClient

  final class BadAbiException( message : String, cause : Throwable = null ) extends ConsuelaException( message, cause )

  final object ClientException {

    final val ShadowAbi = Abi("""[{"name":"Error","inputs":[{"name":"message","type":"string"}],"outputs":[],"type":"function","stateMutability":"pure"}]""")

    def apply( methodDescriptor : String, report : JsonrpcResponse.Error.Report ) : ClientException = {
      val errorCode    = Some( report.code )
      val errorMessage = Some( report.message )
      val errorData    = report.data

      val ( errorDataParseFailureMessage, decodedRevertMessage ) = {

        def extractRevertMessage( fcn : Abi.Function, values : immutable.Seq[Decoded.Value] ) : Failable[String] = Failable.flatCreate {
          if (fcn.name != "Error") {
            Failable.fail(s"Decoded unexpected function from errorData: ${fcn}", includeStackTrace=false)
          }
          else {
            val messages = values.filter( _.parameter.name == "message" )
            require( messages.size == 1, s"Expected precisely one 'message' parameter, found ${messages.size}: " + messages.mkString(",") )
            Failable.succeed( messages.head.stringRep )
          }
        }

        def toHexBytes( s : String ) : Failable[immutable.Seq[Byte]] = Failable.flatCreate {
          if ( s.isEmpty ) {
            Failable.fail( "(Error data was an empty string.)" )
          }
          else {
            Failable( s.decodeHexAsSeq ).recoverWith {
              case Failed( nfe : NumberFormatException ) => Failable.fail( s"(Error data '${s}' was not a decodable hex string.)" )
              case other                                 => other
            }
          }
        }

        errorData match {
          case Some( jss : JsString ) => {
            val f_message = {
              for {
                hexBytes        <- toHexBytes( jss.value )
                ( fcn, values ) <- decodeFunctionCall( ShadowAbi, hexBytes )
                message         <- extractRevertMessage( fcn, values )
              }
              yield {
                message
              }
            }
            f_message match {
              case Succeeded( message ) => ( None, Some( message ) )
              case Failed( source )     => ( Some( source.toString ), None )
            }
          }
          case Some( other ) => {
            ( Some(s"Expected JSON String as error data, found: ${other}"), None ) 
          }
          case None => {
            ( None, None )
          }
        }
      }
      def optionalItem[T]( name : String, item : Option[T] ) = item.map(i => name + "=" + i)
      val basePart = errorMessage.getOrElse("(No error message)")
      val items = optionalItem("decodedRevertMessage", decodedRevertMessage) :: optionalItem( "errorCode", errorCode) :: optionalItem( "rawErrorData", errorData ) :: optionalItem("errorDataParseFailureMessage", errorDataParseFailureMessage) :: Nil
      val itemsPart = " -- " + items.collect{ case Some(s) => s }.mkString("; ")
      val message = basePart + itemsPart + s"; ${methodDescriptor}"

      new ClientException( errorCode, errorMessage, errorData, decodedRevertMessage, message )
    }
  }
  final case class ClientException private (
    val errorCode : Option[Int] = None,
    val errorMessage : Option[String] = None,
    val errorData : Option[JsValue] = None,
    val decodedRevertMessage : Option[String],
    val message : String,
    val cause : Throwable = null
  ) extends ConsuelaException( message, cause )

  private[jsonrpc] def encodeQuantity( quantity : BigInt )  : JsString = JsString( "0x" + quantity.toString(16) )

  private[jsonrpc] def decodeQuantity( encoded : JsString ) : BigInt = decodeQuantity( encoded.value )

  private[jsonrpc] def decodeQuantity( encoded : String ) : BigInt = {
    require( encoded.startsWith("0x"), s"Ethereum JSON-RPC expects hex-encoded quantities to begin with '0x', '${encoded}' does not." )
    BigInt( encoded.substring(2), 16 )
  }

  private[jsonrpc] def encodeBytes( bytes : Seq[Byte] )  : JsString = JsString( "0x" + bytes.hex )

  private[jsonrpc] def decodeBytes( encoded : JsString ) : immutable.Seq[Byte] = decodeBytes( encoded.value )

  private[jsonrpc] def decodeBytes( encoded : String ) : immutable.Seq[Byte] = {
    try {
      encoded.decodeHex.toImmutableSeq
    }
    catch { 
      case e : NumberFormatException => { // handle an annoying special case that shows up in ganache...
        if ( encoded == "0x0" ) Nil else throw e
      }
    }
  }

  private[jsonrpc] def encodeAddress( address : EthAddress ) : JsString = encodeBytes( address.bytes.widen )

  trait MaybeEmpty {
    def isEmpty : Boolean
  }

  implicit val EthLogEntryFormat = new Format[EthLogEntry] {
    def reads( jsv : JsValue ) : JsResult[EthLogEntry] = jsv match {
      case jso : JsObject => {
        try {
          val fields = jso.value
          val address = EthAddress( fields( "address" ).as[String] )
          val topics = fields( "topics" ).as[JsArray].value.map( jsv => ByteSeqExact32( decodeBytes( jsv.as[String] ) ) ).toVector
          val data = decodeBytes( fields( "data" ).as[String] )
          JsSuccess( EthLogEntry( address, topics, data ) )
        } catch {
          case NonFatal( t ) => JsError( t.toString() )
        }
      }
      case _ => JsError( s"EthLogEntry expected as a JsObject, found ${jsv}" )
    }
    def writes( entry : EthLogEntry ) : JsValue = {
      JsObject( immutable.Map( "address" -> encodeBytes( entry.address.bytes.widen ), "topics" -> JsArray( entry.topics.map( topic => encodeBytes( topic.widen ) ) ), "data" -> encodeBytes( entry.data ) ) )
    }
  }

  implicit val EthHashFormat = new Format[EthHash] {
    def reads( jsv : JsValue ) : JsResult[EthHash] = {
      try {
        JsSuccess( EthHash.withBytes( decodeBytes( jsv.as[String] ) ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( hash : EthHash ) : JsValue = encodeBytes( hash.bytes )
  }
  implicit val EthAddressFormat = new Format[EthAddress] {
    def reads( jsv : JsValue ) : JsResult[EthAddress] = {
      try {
        JsSuccess( EthAddress( jsv.as[String] ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( address : EthAddress ) : JsValue = encodeBytes( address.bytes.widen )
  }
  implicit val Unsigned256Format = new Format[Unsigned256] {
    def reads( jsv : JsValue ) : JsResult[Unsigned256] = {
      try {
        JsSuccess( Unsigned256( decodeQuantity( jsv.as[String] ) ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( num : Unsigned256 ) : JsValue = encodeQuantity( num.widen )
  }
  implicit val ClientTransactionReceiptFormat = Json.format[Client.TransactionReceipt]

  implicit val ByteDataFormat = new Format[immutable.Seq[Byte]] {
    def reads( jsv : JsValue ) : JsResult[immutable.Seq[Byte]] = {
      try {
        JsSuccess( decodeBytes( jsv.as[String] ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( data : immutable.Seq[Byte] ) : JsValue = encodeBytes( data )
  }

  // See EthLogEntry.Topic
  implicit val TopicsFormat = new Format[immutable.IndexedSeq[ByteSeqExact32]] {
    def reads( jsv : JsValue ) : JsResult[immutable.IndexedSeq[ByteSeqExact32]] = {
      try {
        JsSuccess( jsv.as[JsArray].value.toVector.map( elem => ByteSeqExact32( decodeBytes( elem.as[String] ) ) ) )
      }
      catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( data : immutable.IndexedSeq[ByteSeqExact32] ) : JsValue = JsArray( data.map( bse => encodeBytes(bse.widen) ) )
  }


  class OptionFormat[T : Format] extends Format[Option[T]] {
    def reads( jsv : JsValue ) : JsResult[Option[T]] = {
      try {
        JsSuccess {
          if ( jsv == JsNull ) None else Some( jsv.as[T] )
        }
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( mbReceipt : Option[T] ) : JsValue = mbReceipt match {
      case Some( receipt ) => Json.toJson( receipt )
      case None            => JsNull
    }
  }

  implicit def toOptionFormat[T : Format] = new OptionFormat[T]

  implicit val UserMethodInfoFormat          = Json.format[Compilation.Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat     = Json.format[Compilation.Doc.Developer.MethodInfo]
  implicit val UserDocFormat                 = Json.format[Compilation.Doc.User]
  implicit val DeveloperDocFormat            = Json.format[Compilation.Doc.Developer]

  implicit val CompilationContractInfoFormat = Json.format[Compilation.Contract.Info]

  // I have no idea why trying to generate this Format automatically was very fragile,
  // sometimes working, sometimes failing
  //
  // The manual version seems to work reliably

  //implicit val CompilationContractFormat = Json.format[Compilation.Contract]

  implicit val CompilationContractFormat : Format[Compilation.Contract] = new Format[Compilation.Contract] {
    def reads( jsv : JsValue ) : JsResult[Compilation.Contract] = {
      try {
        jsv match {
          case jso : JsObject => {
            val code = jso.value.get( "code" ).getOrElse( throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should contain a 'code' key, does not: ${jso}" ) ).as[String]
            val info = jso.value.get( "info" ).getOrElse( throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should contain a 'info' key, does not: ${jso}" ) ).as[Compilation.Contract.Info]
            JsSuccess( Compilation.Contract( code, info ) )
          }
          case other => throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should be a JsObject, is not: ${other}" )
        }
      }
      catch {
        case e : Exception => JsError( e.getMessage() )
      }
    }
    def writes( contract : Compilation.Contract ) : JsValue = JsObject( "code" -> JsString(contract.code) :: "info" -> Json.toJson(contract.info) :: Nil )
  }

  implicit val MapStringCompilationContractFormat = new Format[immutable.Map[String,Compilation.Contract]] {
    def reads( jsv : JsValue ) : JsResult[immutable.Map[String,Compilation.Contract]] = {
      jsv match {
        case jso : JsObject => {
          jso.fields.foldLeft( JsSuccess(immutable.Map.empty[String,Compilation.Contract]) : JsResult[immutable.Map[String,Compilation.Contract]] ){
            (jsr, pair) => jsr.flatMap( map => Json.fromJson[Compilation.Contract](pair._2).flatMap( info => JsSuccess( map + Tuple2( pair._1, info ) ) ) )
          }
        }
        case _ => JsError( s"Map of contract name to Compilation.Contract expected as a JsObject, found ${jsv}" )
      }
    }
    def writes( definition : immutable.Map[String,Compilation.Contract] ) : JsValue = {
      JsObject( definition.map{ case ( k , v ) => ( k , Json.toJson(v) ) } )
    }
  }

  /**
    * A Compilation is just a mapping of String -> Compilation.Contract. We define this as a type
    * definition, in the package object (package.scala), rather than as a separate file, because Scala does not
    * permit top-level type declarations, they must be in a class, trait, or object (but package
    * objects are fine.
    */ 
  object Compilation {

    def opt( str : String ) : Option[String] = if( str.trim == "" ) None else Some( str )

    def str[T : Format]( t : T ) : String = Json.stringify( Json.toJson( t ) )

    def optStr[T <: MaybeEmpty : Format ]( t : T ) : Option[String] = if ( t.isEmpty ) None else Some( str( t ) )

    def opt[T <: MaybeEmpty : Format ]( t : T ) : Option[T] = if ( t.isEmpty ) None else Some( t )

    final object Doc {
      final object User {
        final case class MethodInfo( notice : Option[String] )
      }
      final case class User( methods : Option[immutable.Map[String,User.MethodInfo]] ) extends MaybeEmpty {
        def isEmpty : Boolean = methods.isEmpty || methods.get.isEmpty
      }

      final object Developer {
        final case class MethodInfo( details : Option[String], params : Option[immutable.Map[String,String]] )
      }
      final case class Developer( title : Option[String], methods : Option[immutable.Map[String, Developer.MethodInfo]] ) extends MaybeEmpty {
        def isEmpty : Boolean = title.isEmpty && ( methods.isEmpty || methods.get.isEmpty )
      }
    }

    final object Contract {

      // most fields always have some non-null value, but often it is something uselessly empty
      //
      // metadata is only just proposed to be defined, and in any event would only be available
      // for outputs from very recent compiler versions
      //
      // use the mbXXX methods to get a useful Option (for which the empty values get represented as None)

      final case class Info (
        source          : Option[String],
        language        : Option[String],
        languageVersion : Option[String],
        compilerVersion : Option[String],
        compilerOptions : Option[String],
        abi             : Option[Abi],
        userDoc         : Option[Doc.User],
        developerDoc    : Option[Doc.Developer],
        metadata        : Option[String],
        ast             : Option[String],
        sourceTimestamp : Option[Long]
      ) {
        def mbSource               = source.flatMap( opt )
        def mbLanguage             = language.flatMap( opt )
        def mbLanguageVersion      = languageVersion.flatMap( opt )
        def mbCompilerVersion      = compilerVersion.flatMap( opt )
        def mbCompilerOptions      = compilerOptions.flatMap( opt )
        def mbAbi                  = abi.flatMap( opt[Abi] )
        def mbAbiAsString          = abi.flatMap( optStr[Abi] )
        def mbUserDoc              = userDoc.flatMap( opt[Doc.User] )
        def mbUserDocAsString      = userDoc.flatMap( optStr[Doc.User] )
        def mbDeveloperDoc         = developerDoc.flatMap( opt[Doc.Developer] )
        def mbDeveloperDocAsString = developerDoc.flatMap( optStr[Doc.Developer] )
        def mbMetadata             = metadata.flatMap( opt )
        def mbAst                  = ast.flatMap( opt )
        def mbSourceTimestamp      = sourceTimestamp
      }
    }
    final case class Contract( code : String, info : Contract.Info )
  }
  type Compilation = immutable.Map[String,Compilation.Contract]
}
