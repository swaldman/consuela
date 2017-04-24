package com.mchange.sc.v1.consuela.ethereum


import com.mchange.sc.v1.consuela._

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}

import scala.collection._

import scala.util.control.NonFatal

import play.api.libs.json._

import com.mchange.sc.v2.playjson._

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

  final object Exception {
    def stringifyErrorData( data : JsValue ) : String = {
      data match {
        case JsArray( elems ) => elems.map( stringifyErrorData ).mkString("\n","\n","")
        case str : JsString   => str.as[String]
        case JsNull           => "No further information"
        case whatevs          => Json.stringify( whatevs )
      }
    }
    def stringifyErrorData( data : Option[JsValue] ) : String = {
      data match {
        case Some( jsv ) => stringifyErrorData( jsv )
        case None        => "No further information"
      }
    }
  }
  final class Exception( val code : Int, val message : String, val data : Option[JsValue] = None ) extends EthereumException( s"${message} [code=${code}]: ${Exception.stringifyErrorData(data)}" ) {
    def this( errorResponse : Response.Error ) = this( errorResponse.error.code, errorResponse.error.message, errorResponse.error.data ) 
  }

  type Response = Either[Response.Error,Response.Success]

  final object Compilation {

    def opt( str : String ) : Option[String] = if( str.trim == "" ) None else Some( str )

    def str[T : Format]( t : T ) : String = Json.stringify( Json.toJson( t ) )

    def opt[T <: MaybeEmpty : Format ]( t : T ) : Option[String] = if ( t.isEmpty ) None else Some( str( t ) )

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
        abiDefinition   : Option[Abi.Definition],
        userDoc         : Option[Doc.User],
        developerDoc    : Option[Doc.Developer],
        metadata        : Option[String]
      ) {
        def mbSource          = source.flatMap( opt )
        def mbLanguage        = language.flatMap( opt )
        def mbLanguageVersion = languageVersion.flatMap( opt )
        def mbCompilerVersion = compilerVersion.flatMap( opt )
        def mbCompilerOptions = compilerOptions.flatMap( opt )
        def mbAbiDefinition   = abiDefinition.flatMap( opt[Abi.Definition] )
        def mbUserDoc         = userDoc.flatMap( opt[Doc.User] )
        def mbDeveloperDoc    = developerDoc.flatMap( opt[Doc.Developer] )
        def mbMetadata        = metadata.flatMap( opt )
      }
    }
    final case class Contract( code : String, info : Contract.Info )
  }
  type Compilation = immutable.Map[String,Compilation.Contract]

  trait MaybeEmpty {
    def isEmpty : Boolean
  }

  final object Abi {
    object Definition {
      val empty = Definition( immutable.Seq.empty, immutable.Seq.empty, immutable.Seq.empty, None )
    }
    final case class Definition( functions : immutable.Seq[Function], events : immutable.Seq[Event], constructors : immutable.Seq[Constructor], fallback : Option[Fallback] ) extends MaybeEmpty {
      def isEmpty : Boolean = functions.isEmpty && events.isEmpty && constructors.isEmpty
    }

    object Function {
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean, payable : Boolean )

    object Constructor {
      val noArg = Constructor( Nil )
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Constructor( inputs : immutable.Seq[Function.Parameter] )

    object Event {
      final case class Parameter( name : String, `type` : String, indexed : Boolean ) extends Abi.Parameter
    }
    final case class Event( name : String, inputs : immutable.Seq[Event.Parameter], anonymous : Boolean )

    final case class Fallback( payable : Boolean )

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

  private val DefaultTrue  = Some( JsBoolean( true ) )
  private val DefaultFalse = Some( JsBoolean( false ) )

  private def rd[T]( spec : (String,Option[JsValue])* )( inner : Format[T] ) : Format[T] = new RestrictingDefaultingFormat( spec.toMap )( inner )

  /*
   *  The defaults here are meant to fill in for attributes not omitted by earlier versions of the solidity compiler.
   * 
   *  In older versions, all functions were payable, so that defaults to true.
   *                     there were no anonymous events, so that defaults to false.
   * 
   *  Note that "payable" would more sensibly default to false, and indeed it does in newer compilers, 
   *  which emit payable as false.
   * 
   *  But ABI generated by older compilers always omitted the (then-undefined) modifier, and the behavior was to
   *  always accept payment. So our default, if no payable field is present, is to consider "payable" true, so that
   *  we do not alter or break behavior when we read and then rewrite ABI.
   */              
  implicit val AbiFunctionParameterFormat    = rd( "name" -> None, "type" -> None )                                                                                  ( Json.format[Abi.Function.Parameter]    )
  implicit val AbiFunctionFormat             = rd( "name" -> None, "inputs" -> None, "outputs" -> None, "constant"-> None, "payable" -> DefaultTrue, "type" -> None )( Json.format[Abi.Function]              )
  implicit val AbiEventParameterFormat       = rd( "name" -> None, "type" -> None, "indexed" -> None )                                                               ( Json.format[Abi.Event.Parameter]       )
  implicit val AbiEventFormat                = rd( "name" -> None, "inputs" -> None, "anonymous" -> DefaultFalse, "type" -> None )                                   ( Json.format[Abi.Event]                 )
  implicit val AbiConstructorParameterFormat = rd( "name" -> None, "type" -> None )                                                                                  ( Json.format[Abi.Constructor.Parameter] )
  implicit val AbiConstructorFormat          = rd( "inputs" -> None, "payable" -> DefaultTrue, "type" -> None )                                                      ( Json.format[Abi.Constructor]           )
  implicit val AbiFallbackFormat             = rd( "payable" -> DefaultTrue, "type" -> None )                                                                        ( Json.format[Abi.Fallback]              )

  implicit val UserMethodInfoFormat          = Json.format[Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat     = Json.format[Doc.Developer.MethodInfo]
  implicit val UserDocFormat                 = Json.format[Doc.User]
  implicit val DeveloperDocFormat            = Json.format[Doc.Developer]

  // these we'll have to do ourselves
  implicit val AbiDefinitionFormat : Format[Abi.Definition] = new Format[Abi.Definition] {
    def reads( jsv : JsValue ) : JsResult[Abi.Definition] = {
      jsv match {
        case jsa : JsArray => {
          val ( functions, events, constructors, fallback, message ) = {
            def accumulate(
              tuple : Tuple5[List[JsValue],List[JsValue],List[JsValue],Option[JsValue],Option[String]],
              elem : JsValue
            ) : Tuple5[List[JsValue],List[JsValue],List[JsValue],Option[JsValue],Option[String]] = { 
              val ( fs, es, cs, fb, m ) = tuple
              m match {
                case Some( _ ) => ( fs, es, cs, fb, m )
                case None      => {
                  try {
                    (elem \ "type").get.as[String] match {
                      case "function"    => ( elem :: fs, es, cs, fb, m )
                      case "event"       => ( fs, elem :: es, cs, fb, m )
                      case "constructor" => ( fs, es, elem :: cs, fb, m )
                      case "fallback"    => {
                        fb match {
                          case Some( _ ) => ( fs, es, cs, fb, Some( s"Two fallbacks found, only one permitted: $jsa" ) )
                          case None      => ( fs, es, cs, Some( elem ), m )
                        }
                      }
                      case unexpected    => ( fs, es, cs, fb, Some( s"Unexpected element type in ABI: $unexpected" ) )
                    }
                  } catch {
                    case nse : NoSuchElementException => ( elem :: fs, es, cs, fb, m ) // spec says default is function
                  }
                }
              }
            }
            jsa.value.foldLeft( ( Nil, Nil, Nil, None, None ) : Tuple5[List[JsValue],List[JsValue],List[JsValue],Option[JsValue],Option[String]] ) ( accumulate )
          }
          message match {
            case None => {
              val abi = Abi.Definition( functions.reverse.map( _.as[Abi.Function] ), events.reverse.map( _.as[Abi.Event] ), constructors.reverse.map( _.as[Abi.Constructor] ), fallback.map( _.as[Abi.Fallback] ) )
              JsSuccess( abi )
            }
            case Some( words ) => JsError( words )
          }
        }
        case _ => JsError( s"abiDefinition is expected as a JsArray, found ${jsv}" )
      }
    }
    def writes( definition : Abi.Definition ) : JsValue = {
      def makeFunction( abif : Abi.Function )       = Json.toJson(abif).asInstanceOf[JsObject] + ( "type", JsString("function") )
      def makeEvent( abie : Abi.Event )             = Json.toJson(abie).asInstanceOf[JsObject] + ( "type", JsString("event") )
      def makeConstructor( abic : Abi.Constructor ) = Json.toJson(abic).asInstanceOf[JsObject] + ( "type", JsString("constructor") )
      def makeFallback( abifb : Abi.Fallback )      = Json.toJson(abifb).asInstanceOf[JsObject] + ( "type", JsString("fallback") )
      JsArray(
        immutable.Seq.empty[JsValue] ++ definition.functions.map( makeFunction ) ++ definition.events.map( makeEvent ) ++ definition.constructors.map( makeConstructor ) ++ definition.fallback.toSeq.map( makeFallback )
      )
    }
  }

  implicit val CompilationContractInfoFormat = Json.format[Compilation.Contract.Info]
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
