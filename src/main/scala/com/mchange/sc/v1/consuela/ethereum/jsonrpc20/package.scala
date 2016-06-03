package com.mchange.sc.v1.consuela.ethereum

import scala.collection._

import play.api.libs.json._

import com.mchange.sc.v2.failable._
import com.mchange.leftright._

package object jsonrpc20 extends BiasedEither.RightBias.Base[Response.Error]( Response.Error.Empty ) {

  type Response = Either[Response.Error,Response.Success]

  final object Compilation {
    final case class Info (
      source          : String,
      language        : String,
      languageVersion : String,
      compilerVersion : String,
      compilerOptions : String,
      abiDefinition   : Abi.Definition,
      userDoc         : Doc.User,
      developerDoc    : Doc.Developer
    )

    final case class Contract( code : String, info : Info )

    //def apply( jso : JsObject ) : Option[Compilation] = Json.fromJson[immutable.Map[String,Compilation.Contract]]( jso ).asOpt.map( this.apply )
  }
  //final case class Compilation( contracts : immutable.Map[String,Compilation.Contract] )

  final object Abi {
    final case class Definition( functions : immutable.Seq[Function], events : immutable.Seq[Event] )

    object Function {
      case class Parameter( name : String, `type` : String ) extends Abi.Parameter
    }
    final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean )

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
    final case class User( methods : Option[immutable.Map[String,User.MethodInfo]] )

    final object Developer {
      final case class MethodInfo( details : Option[String], params : Option[immutable.Map[String,String]] )
    }
    final case class Developer( title : Option[String], methods : Option[immutable.Map[String, Developer.MethodInfo]] )
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

  implicit val AbiFunctionParameterFormat = Json.format[Abi.Function.Parameter]
  implicit val AbiFunctionFormat          = Json.format[Abi.Function]
  implicit val AbiEventParameterFormat    = Json.format[Abi.Event.Parameter]
  implicit val AbiEventFormat             = Json.format[Abi.Event]
  implicit val UserMethodInfoFormat       = Json.format[Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat  = Json.format[Doc.Developer.MethodInfo]
  implicit val UserDocFormat              = Json.format[Doc.User]
  implicit val DeveloperDocFormat         = Json.format[Doc.Developer]

  // these we'll have to do ourselves
  implicit val AbiDefinitionFormat : Format[Abi.Definition] = new Format[Abi.Definition] {
    def reads( jsv : JsValue ) : JsResult[Abi.Definition] = {
      jsv match {
        case jsa : JsArray => {
          try {
            val ( functions, events ) = jsa.value.partition( jsv => ((jsv \ "type").get.as[String] == "function") ) // asserts existence of "type", or NoSuchElementException
            JsSuccess( Abi.Definition( functions.map( _.as[Abi.Function] ).toVector, events.map( _.as[Abi.Event] ).toVector ) )
          } catch {
            case nse : NoSuchElementException => JsError( s"Failed to find a binding with key 'type' in some element of ${jsa}." )
          }
        }
        case _ => JsError( s"abiDefinition is expected as a JsArray, found ${jsv}" )
      }
    }
    def writes( definition : Abi.Definition ) : JsValue = {
      def makeFunction( abif : Abi.Function ) = Json.toJson(abif).asInstanceOf[JsObject] + ( "type", JsString("function") )
      def makeEvent( abie : Abi.Event )       = Json.toJson(abie).asInstanceOf[JsObject] + ( "type", JsString("event") )
      JsArray( immutable.Seq.empty[JsValue] ++ definition.functions.map( makeFunction ) ++ definition.events.map( makeEvent ) )
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

  implicit final object ErrorResponseAsFailSource extends FailSource[Response.Error] {
    def getMessage( source : Response.Error ) : String = source.error.message;
  }
  def toFailable( response : Response ) : Failable[Response.Success] = response.xmap( ErrorResponseAsFailSource.getFail( _ ) )

}
