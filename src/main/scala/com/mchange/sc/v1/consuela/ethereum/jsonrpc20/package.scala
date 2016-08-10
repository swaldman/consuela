package com.mchange.sc.v1.consuela.ethereum

import com.mchange.sc.v1.consuela._

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}

import scala.collection._

import scala.util.control.NonFatal

import play.api.libs.json._

import com.mchange.leftright._

package object jsonrpc20 extends BiasedEither.RightBias.Base[Response.Error]( Response.Error.Empty ) {

  private[jsonrpc20] def encodeQuantity( quantity : BigInt )  : JsString = JsString( "0x" + quantity.toString(16) )

  private[jsonrpc20] def decodeQuantity( encoded : JsString ) : BigInt = decodeQuantity( encoded.value )

  private[jsonrpc20] def decodeQuantity( encoded : String ) : BigInt = {
    require( encoded.startsWith("0x"), s"Ethereum JSON-RPC expects hex-encoded quantities to begin with '0x', '${encoded}' does not." )
    BigInt( encoded.substring(2), 16 )
  }

  private[jsonrpc20] def encodeBytes( bytes : Seq[Byte] )  : JsString = JsString( "0x" + bytes.hex )

  private[jsonrpc20] def decodeBytes( encoded : JsString ) : immutable.Seq[Byte] = decodeBytes( encoded.value )

  private[jsonrpc20] def decodeBytes( encoded : String ) : immutable.Seq[Byte] = encoded.decodeHex.toImmutableSeq

  private[jsonrpc20] def encodeAddress( address : EthAddress ) : JsString = encodeBytes( address.bytes.widen )

  final class Exception( val code : Int, val message : String ) extends EthereumException( s"${message} [code=${code}]" ) {
    def this( errorResponse : Response.Error ) = this( errorResponse.error.code, errorResponse.error.message ) 
  }

  type Response = Either[Response.Error,Response.Success]

  final object Compilation {

    def opt( str : String ) : Option[String] = if( str.trim == "" ) None else Some( str )

    def str[T : Format]( t : T ) : String = Json.stringify( Json.toJson( t ) )

    def opt[T <: MaybeEmpty : Format ]( t : T ) : Option[String] = if ( t.isEmpty ) None else Some( str( t ) )


    final case class Info (
      source          : String,
      language        : String,
      languageVersion : String,
      compilerVersion : String,
      compilerOptions : String,
      abiDefinition   : Abi.Definition,
      userDoc         : Doc.User,
      developerDoc    : Doc.Developer
    ) {
      def mbSource          = opt( source )
      def mbLanguage        = opt( language )
      def mbLanguageVersion = opt( languageVersion )
      def mbCompilerVersion = opt( compilerVersion )
      def mbCompilerOptions = opt( compilerOptions )
      def mbAbiDefinition   = opt( abiDefinition )
      def mbUserDoc         = opt( userDoc )
      def mbDeveloperDoc    = opt( developerDoc )
    }

    final case class Contract( code : String, info : Info )

    //def apply( jso : JsObject ) : Option[Compilation] = Json.fromJson[immutable.Map[String,Compilation.Contract]]( jso ).asOpt.map( this.apply )
  }
  //final case class Compilation( contracts : immutable.Map[String,Compilation.Contract] )

  trait MaybeEmpty {
    def isEmpty : Boolean
  }

  final object Abi {
    final case class Definition( functions : immutable.Seq[Function], events : immutable.Seq[Event], constructors : immutable.Seq[Constructor] ) extends MaybeEmpty {
      def isEmpty : Boolean = functions.isEmpty && events.isEmpty && constructors.isEmpty
    }

    object Function {
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean )

    object Constructor {
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Constructor( inputs : immutable.Seq[Function.Parameter] )

    object Event {
      final case class Parameter( name : String, `type` : String, indexed : Boolean ) extends Abi.Parameter
    }
    final case class Event( name : String, inputs : immutable.Seq[Event.Parameter], anonymous : Boolean )

    sealed trait Parameter {
      val name : String
      val `type`  : String
      def tpe = `type`
    }
  }

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

  implicit val SuccessResponseFormat = Json.format[Response.Success]

  implicit val ErrorReportFormat   = Json.format[Response.Error.Report]
  implicit val ErrorResponseFormat = Json.format[Response.Error]

  implicit val ResponseFormat : Format[Response] = new Format[Response] {
    def reads( jsv : JsValue ) : JsResult[Response] = {
      jsv match {
        case jso : JsObject if jso.keys("result") => SuccessResponseFormat.reads( jso ).map( Right(_) )
        case jso : JsObject if jso.keys("error")  => ErrorResponseFormat.reads( jso ).map( Left(_) )
        case jso : JsObject                       => JsError( s"Response is expected to contain either a 'result' or 'error' field" )
        case _                                    => JsError( s"Response is expected as a JsObject, found ${jsv}" )
      }
    }
    def writes( response : Response ) : JsValue = response match {
      case Left( errorResponse ) => ErrorResponseFormat.writes( errorResponse )
      case Right( goodResponse ) => SuccessResponseFormat.writes( goodResponse )
    }
  }

  implicit val AbiFunctionParameterFormat    = Json.format[Abi.Function.Parameter]
  implicit val AbiFunctionFormat             = Json.format[Abi.Function]
  implicit val AbiEventParameterFormat       = Json.format[Abi.Event.Parameter]
  implicit val AbiEventFormat                = Json.format[Abi.Event]
  implicit val AbiConstructorParameterFormat = Json.format[Abi.Constructor.Parameter]
  implicit val AbiConstructorFormat          = Json.format[Abi.Constructor]
  implicit val UserMethodInfoFormat          = Json.format[Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat     = Json.format[Doc.Developer.MethodInfo]
  implicit val UserDocFormat                 = Json.format[Doc.User]
  implicit val DeveloperDocFormat            = Json.format[Doc.Developer]

  // these we'll have to do ourselves
  implicit val AbiDefinitionFormat : Format[Abi.Definition] = new Format[Abi.Definition] {
    def reads( jsv : JsValue ) : JsResult[Abi.Definition] = {
      jsv match {
        case jsa : JsArray => {
          val ( functions, events, constructors ) = {
            def accumulate( tuple : Tuple3[List[JsValue],List[JsValue],List[JsValue]], jsv : JsValue ) : Tuple3[List[JsValue],List[JsValue],List[JsValue]] = { // ( functions, events, constructors )
              try {
                // MatchError for now if there is a specified, but unknown, type
                (jsv \ "type").get.as[String] match {
                  case "function"    => ( jsv :: tuple._1, tuple._2, tuple._3 )
                  case "event"       => ( tuple._1, jsv :: tuple._2, tuple._3 )
                  case "constructor" => ( tuple._1, tuple._2, jsv :: tuple._3 )
                }
              } catch {
                case nse : NoSuchElementException => ( jsv :: tuple._1, tuple._2, tuple._3 ) // spec says default is function
              }
            }
            jsa.value.foldLeft( ( Nil, Nil, Nil ) : Tuple3[List[JsValue],List[JsValue],List[JsValue]] )( accumulate )
          }
          JsSuccess( Abi.Definition( functions.reverse.map( _.as[Abi.Function] ), events.reverse.map( _.as[Abi.Event] ), constructors.reverse.map( _.as[Abi.Constructor] ) ) )
        }
        case _ => JsError( s"abiDefinition is expected as a JsArray, found ${jsv}" )
      }
    }
    def writes( definition : Abi.Definition ) : JsValue = {
      def makeFunction( abif : Abi.Function )       = Json.toJson(abif).asInstanceOf[JsObject] + ( "type", JsString("function") )
      def makeEvent( abie : Abi.Event )             = Json.toJson(abie).asInstanceOf[JsObject] + ( "type", JsString("event") )
      def makeConstructor( abic : Abi.Constructor ) = Json.toJson(abic).asInstanceOf[JsObject] + ( "type", JsString("constructor") )
      JsArray( immutable.Seq.empty[JsValue] ++ definition.functions.map( makeFunction ) ++ definition.events.map( makeEvent ) ++ definition.constructors.map( makeConstructor ) )
    }
  }

  implicit val CompilationInfoFormat = Json.format[Compilation.Info]
  implicit val CompilationContractFormat = Json.format[Compilation.Contract]

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

  case class ClientTransactionReceipt (
    transactionHash   : EthHash,
    transactionIndex  : Unsigned256,
    blockHash         : EthHash,
    blockNumber       : Unsigned256,
    cumulativeGasUsed : Unsigned256,
    gasUsed           : Unsigned256,
    contractAddress   : Option[EthAddress],
    logs              : immutable.Seq[EthLogEntry]
  )


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
  implicit val ClientTransactionReceiptFormat = Json.format[ClientTransactionReceipt]

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
}
