package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import scala.collection._
import play.api.libs.json._

final object LooseAbi {
  import Ordering.Implicits._
  implicit val Ordering_FunctionParameter    = Ordering.by( (fp : Function.Parameter)    => (fp.name, fp.`type`, fp.internalType)                                               )
  implicit val Ordering_ConstructorParameter = Ordering.by( (cp : Constructor.Parameter) => (cp.name, cp.`type`, cp.internalType)                                               )
  implicit val Ordering_EventParameter       = Ordering.by( (ep : Event.Parameter)       => (ep.name, ep.`type`, ep.internalType, ep.indexed)                                   )
  implicit val Ordering_Function             = Ordering.by( (fcn : Function)             => (fcn.name, fcn.inputs, fcn.outputs, fcn.constant, fcn.payable, fcn.stateMutability) )
  implicit val Ordering_Constructor          = Ordering.by( (ctor : Constructor)         => (ctor.inputs, ctor.payable, ctor.stateMutability)                                   )
  implicit val Ordering_Event                = Ordering.by( (ev : Event)                 => (ev.name, ev.inputs, ev.anonymous)                                                  )

  def apply( json : JsValue ) : LooseAbi = {
    json match {
      case jsa : JsArray => apply( jsa )
      case _             => throw new BadAbiException( s"An ABI must be a JSON vector/array, found: ${json}" )
    }
  }
  lazy val empty : LooseAbi = LooseAbi( JsArray.empty )

  final object Function {
    case class Parameter( json : JsObject ) {
      lazy val name         : String         = ???
      lazy val `type`       : String         = ???
      lazy val internalType : Option[String] = ???

      lazy val sorted = Function.Parameter( JsObject( SortedMap.empty[String,JsValue] ++ json.value ) )
    }
  }
  final case class Function( json : JsObject ) {
    lazy val name            : String                            = ???
    lazy val inputs          : immutable.Seq[Function.Parameter] = ???
    lazy val outputs         : immutable.Seq[Function.Parameter] = ???
    lazy val constant        : Boolean                           = ???
    lazy val payable         : Option[Boolean]                   = ???
    lazy val stateMutability : Option[String]                    = ???

    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      val sortedOutputs = JsArray( outputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) + ("outputs" -> sortedOutputs) ).toSeq ) )
    }
  }
  final object Constructor {
    val noArgNoEffect : Constructor = ???
    final case class Parameter( json : JsObject ) {
      lazy val name         : String = ???
      lazy val `type`       : String = ???
      lazy val internalType : Option[String] = ???

      lazy val sorted = Constructor.Parameter( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
    }
  }
  final case class Constructor( json : JsObject ) {
    lazy val inputs          : immutable.Seq[Function.Parameter] = ???
    lazy val payable         : Boolean                           = ???
    lazy val stateMutability : String                            = ???

    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) ).toSeq ) )
    }
  }
  final object Event {
    final case class Parameter( json : JsObject ) {
      lazy val name         : String         = ???
      lazy val `type`       : String         = ???
      lazy val indexed      : Boolean        = ???
      lazy val internalType : Option[String] = ???

      lazy val sorted = Event.Parameter( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
    }
  }
  final case class Event( json : JsObject ) {
    lazy val name      : String                         = ???
    lazy val inputs    : immutable.Seq[Event.Parameter] = ???
    lazy val anonymous : Boolean                        = ???

    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) ).toSeq ) )
    }
  }
  final case class Receive( json : JsObject ) {
    lazy val stateMutability : Option[String] = ???

    lazy val sorted = Receive( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
  }
  final case class Fallback( json : JsObject ) {
    lazy val payable         : Boolean        = ???
    lazy val stateMutability : Option[String] = ???

    lazy val sorted = Fallback( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
  }
}
case class LooseAbi( json : JsArray ) extends MaybeEmpty {
  lazy val functions    : immutable.Seq[LooseAbi.Function]    = ???
  lazy val events       : immutable.Seq[LooseAbi.Event]       = ???
  lazy val constructors : immutable.Seq[LooseAbi.Constructor] = ???
  lazy val receive      : Option[LooseAbi.Receive]            = ???
  lazy val fallback     : Option[LooseAbi.Fallback]           = ???

  lazy val unexpected : immutable.IndexedSeq[JsValue] = {
    json.value.filter {
      case jso : JsObject => {
        val mbType = jso.value.get("type")
        mbType match {
          case Some( JsString( "function" | "event" | "constructor" | "receive" | "fallback" ) ) => false // these are expected, not unexpected
          case _                                                                                 => true
        }
      }
      case _ => true
    }.toVector
  }

  lazy val sorted = {
    LooseAbi(
      JsArray(
        immutable.IndexedSeq.empty[JsValue]                 ++
          functions.map( _.sorted ).sorted.map( _.json )    ++
          events.map( _.sorted).sorted.map( _.json )        ++
          constructors.map( _.sorted ).sorted.map( _.json ) ++
          receive.map( _.json )                             ++
          fallback.map( _.json )                            ++
          unexpected
      )
    )
  }

  def isEmpty : Boolean = json.value.isEmpty
  def withStandardSort : LooseAbi = sorted
}
