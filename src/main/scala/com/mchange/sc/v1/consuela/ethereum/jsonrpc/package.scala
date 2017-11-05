package com.mchange.sc.v1.consuela.ethereum


import com.mchange.sc.v1.consuela._

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}

import scala.collection._

import scala.util.control.NonFatal

import play.api.libs.json._

import com.mchange.sc.v2.playjson._

package object jsonrpc {

  private[jsonrpc] def encodeQuantity( quantity : BigInt )  : JsString = JsString( "0x" + quantity.toString(16) )

  private[jsonrpc] def decodeQuantity( encoded : JsString ) : BigInt = decodeQuantity( encoded.value )

  private[jsonrpc] def decodeQuantity( encoded : String ) : BigInt = {
    require( encoded.startsWith("0x"), s"Ethereum JSON-RPC expects hex-encoded quantities to begin with '0x', '${encoded}' does not." )
    BigInt( encoded.substring(2), 16 )
  }

  private[jsonrpc] def encodeBytes( bytes : Seq[Byte] )  : JsString = JsString( "0x" + bytes.hex )

  private[jsonrpc] def decodeBytes( encoded : JsString ) : immutable.Seq[Byte] = decodeBytes( encoded.value )

  private[jsonrpc] def decodeBytes( encoded : String ) : immutable.Seq[Byte] = encoded.decodeHex.toImmutableSeq

  private[jsonrpc] def encodeAddress( address : EthAddress ) : JsString = encodeBytes( address.bytes.widen )

  final object Compilation {

    def opt( str : String ) : Option[String] = if( str.trim == "" ) None else Some( str )

    def str[T : Format]( t : T ) : String = Json.stringify( Json.toJson( t ) )

    def optStr[T <: MaybeEmpty : Format ]( t : T ) : Option[String] = if ( t.isEmpty ) None else Some( str( t ) )

    def opt[T <: MaybeEmpty : Format ]( t : T ) : Option[T] = if ( t.isEmpty ) None else Some( t )

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
        metadata        : Option[String]
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
      }
    }
    final case class Contract( code : String, info : Contract.Info )
  }
  type Compilation = immutable.Map[String,Compilation.Contract]

  trait MaybeEmpty {
    def isEmpty : Boolean
  }

  final object Abi {
    def apply( json : String ) : Abi = Json.parse( json ).as[Abi]
    val empty = Abi( immutable.Seq.empty, immutable.Seq.empty, immutable.Seq.empty, None )

    object Function {
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean, payable : Boolean, stateMutability : String )

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
  final case class Abi(
    functions : immutable.Seq[Abi.Function],
    events : immutable.Seq[Abi.Event],
    constructors : immutable.Seq[Abi.Constructor],
    fallback : Option[Abi.Fallback]
  ) extends MaybeEmpty {
    def isEmpty : Boolean = functions.isEmpty && events.isEmpty && constructors.isEmpty
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

  /*
   *  The defaults here are meant to fill in for attributes not emitted by earlier versions of the solidity compiler.
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
  private val DefaultTrue  = Some( JsBoolean( true ) )
  private val DefaultFalse = Some( JsBoolean( false ) )

  private def restrictTransformAbiFunctionValue( jsv : JsValue ) : JsResult[JsObject] = {
    jsv match {
      case jso : JsObject => restrictTransformAbiFunctionObject( jso )
      case oops           => JsError( s"An abi function must be a JsObject, found $oops" )
    }
  }

  private def restrictTransformAbiFunctionObject( jso : JsObject ) : JsResult[JsObject] = {
    val keys = jso.keys

    // these keys required for all versions
    val requiredKeys = "name" :: "inputs" :: "outputs" :: "type" :: "constant" :: Nil

    def requiredKeysCheck = requiredKeys.foldLeft( None : Option[JsResult[JsObject]] ) { ( nascent, next ) =>
      nascent match {
        case Some( _ ) => nascent
        case None      => if (!keys(next)) Some( JsError(s"Required key for ABI function '${next}' not found.") ) else None
      }
    }
    def constantValue : Boolean = jso.value("constant").asInstanceOf[JsBoolean].value
    def payableValue  : Boolean = jso.value("payable").asInstanceOf[JsBoolean].value

    def informalAbiVersion = {
      val hasPayable         = keys("payable")
      val hasStateMutability = keys("stateMutability")

      (hasPayable, hasStateMutability) match {
        case ( false, false ) => 1
        case ( true, false  ) => 2
        case ( true, true   ) => 3
        case ( false, true  ) => 4 // we're required to fill in the payable field from stateMutability
      }
    }
    def constructBaseObject : JsObject = {
      requiredKeys.foldLeft(JsObject(Nil)){ ( nascent, next ) => nascent + (next, jso.value(next)) }
    }
    def augmentForInformalAbiVersion( informalAbiVersion : Int )( baseObject : JsObject ) : JsResult[JsObject] = {
      informalAbiVersion match {
        case 1 => {
          if ( constantValue ) {
            JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( false )) + ("stateMutability", JsString("view")) )
          } else {
            JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( true )) + ("stateMutability", JsString("payable")) ) // maintain compatability, under old compilations all functions payable
          }
        }
        case 2 => {
          if ( payableValue ) {
            JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( true )) + ("stateMutability", JsString("payable")) )
          }
          else {
            if (constantValue) {
              JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( false )) + ("stateMutability", JsString("view")) ) // we can't guarantee pure, so we call this view
            }
            else {
              JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( false )) + ("stateMutability", JsString("nonpayable")) )
            }
          }
        }
        case 3 => {
          JsSuccess[JsObject]( Seq( "payable", "stateMutability" ).foldLeft( baseObject ){ ( nascent, next ) => nascent + ( next, jso.value(next) ) } )
        }
        case 4 => {
          val sm = jso.value("stateMutability").asInstanceOf[JsString]
          JsSuccess[JsObject]( baseObject + ("payable", JsBoolean( sm.value == "payable" )) + ("stateMutability", sm) )
        }
        case _ => throw new InternalError("Unexpected 'Informal ABI Version', this should never happen!")
      }
    }
    requiredKeysCheck.getOrElse( augmentForInformalAbiVersion( informalAbiVersion )( constructBaseObject ) )
  }

  private def rd[T]( spec : (String,Option[JsValue])* )( inner : Format[T] ) : Format[T] = new RestrictingDefaultingFormat( spec.toMap )( inner )

  implicit val AbiFunctionParameterFormat    = rd( "name" -> None, "type" -> None )                                                ( Json.format[Abi.Function.Parameter]    )
  implicit val AbiFunctionFormat             = new RestrictTransformingFormat( Seq( restrictTransformAbiFunctionValue ) )         ( Json.format[Abi.Function]              )
  implicit val AbiEventParameterFormat       = rd( "name" -> None, "type" -> None, "indexed" -> None )                             ( Json.format[Abi.Event.Parameter]       )
  implicit val AbiEventFormat                = rd( "name" -> None, "inputs" -> None, "anonymous" -> DefaultFalse, "type" -> None ) ( Json.format[Abi.Event]                 )
  implicit val AbiConstructorParameterFormat = rd( "name" -> None, "type" -> None )                                                ( Json.format[Abi.Constructor.Parameter] )
  implicit val AbiConstructorFormat          = rd( "inputs" -> None, "payable" -> DefaultTrue, "type" -> None )                    ( Json.format[Abi.Constructor]           )
  implicit val AbiFallbackFormat             = rd( "payable" -> DefaultTrue, "type" -> None )                                      ( Json.format[Abi.Fallback]              )

  implicit val UserMethodInfoFormat          = Json.format[Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat     = Json.format[Doc.Developer.MethodInfo]
  implicit val UserDocFormat                 = Json.format[Doc.User]
  implicit val DeveloperDocFormat            = Json.format[Doc.Developer]

  // these we'll have to do ourselves
  implicit val AbiFormat : Format[Abi] = new Format[Abi] {
    def reads( jsv : JsValue ) : JsResult[Abi] = {
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
              val abi = Abi( functions.reverse.map( _.as[Abi.Function] ), events.reverse.map( _.as[Abi.Event] ), constructors.reverse.map( _.as[Abi.Constructor] ), fallback.map( _.as[Abi.Fallback] ) )
              JsSuccess( abi )
            }
            case Some( words ) => JsError( words )
          }
        }
        case _ => JsError( s"abi is expected as a JsArray, found ${jsv}" )
      }
    }
    def writes( definition : Abi ) : JsValue = {
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
    logs              : immutable.Seq[EthLogEntry],
    root              : Option[EthHash],
    status            : Option[Unsigned256]
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
