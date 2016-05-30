package com.mchange.sc.v1.consuela.ethereum

import scala.collection._

// import play.api.libs.json._

package object jsonrpc20 {
  //final case class Response( id : Int, result : JsObject );

  final case class CompilationInfo (
    source          : String,
    language        : String,
    languageVersion : String,
    compilerVersion : String,
    compilerOptions : String,
    abi             : Abi.Definition,
    userDoc         : Doc.User,
    developerDoc    : Doc.Developer
  )

  final object Abi {
    final case class Definition( functions : immutable.Seq[Function], events : immutable.Seq[Event] )

    object Function {
      case class Parameter( name : String, tpe : String ) extends Abi.Parameter
    }
    final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean )

    object Event {
      final case class Parameter( name : String, tpe : String, indexed : Boolean ) extends Abi.Parameter
    }
    final case class Event( name : String, inputs : immutable.Seq[Event.Parameter], anonymous : Boolean )

    sealed trait Parameter {
      val name : String
      val tpe  : String
    }
  }

  final object Doc {
    final object User {

      /*
      implicit final object Formatter extends Format[Doc.User] {
        def reads(json : JsValue): JsResult[Doc.User] = {
          json match {
            case jso : JsObject =>
            case _ => JsError(  
          }
        }
      }
      */ 

      final case class MethodInfo( notice : String )
    }
    final case class User( methods : immutable.Map[String,User.MethodInfo] )

    final object Developer {
      final case class MethodInfo( details : String, params : immutable.Map[String,String] )
    }
    final case class Developer( title : String, methods : immutable.Map[String, Developer.MethodInfo] )
  }
}
