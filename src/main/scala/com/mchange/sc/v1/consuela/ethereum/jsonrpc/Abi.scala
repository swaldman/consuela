package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import scala.collection._
import play.api.libs.json._

final object Abi {
  def apply( json : String ) : Abi = Json.parse( json ).as[Abi]
  val empty = Abi( immutable.Seq.empty, immutable.Seq.empty, immutable.Seq.empty, None, None )

  final object Function {
    case class Parameter( name : String, `type` : String, internalType : String ) extends Abi.Parameter
  }
  final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean, payable : Boolean, stateMutability : String )

  final object Constructor {
    val noArgNoEffect = Constructor( Nil, false, "pure" )
    case class Parameter( name : String, `type` : String, internalType : String ) extends Abi.Parameter
  }
  final case class Constructor( inputs : immutable.Seq[Constructor.Parameter], payable : Boolean, stateMutability : String )

  final object Event {
    final case class Parameter( name : String, `type` : String, indexed : Boolean, internalType : String ) extends Abi.Parameter
  }
  final case class Event( name : String, inputs : immutable.Seq[Event.Parameter], anonymous : Boolean )

  final case class Receive( stateMutability : String ) {
    require( stateMutability == "payable", s"Receive functions should always have stateMutability 'payable', found '${stateMutability}'." )
  }

  final case class Fallback( payable : Boolean, stateMutability : String ) {
    def this( payable : Boolean ) = this( payable, if (payable) "payable" else "nonpayable" )
  }

  sealed trait Parameter {
    val name : String
    val `type`  : String
    val internalType : String
    def tpe = `type`
  }

  private final object StandardSort {
    import Ordering.Implicits._
    implicit val Ordering_FunctionParameter = Ordering.by( (fp : Function.Parameter) => (fp.name, fp.`type`, fp.internalType) )
    implicit val Ordering_ConstructorParameter = Ordering.by( (cp : Constructor.Parameter) => (cp.name, cp.`type`, cp.internalType) )
    implicit val Ordering_EventParameter = Ordering.by( (ep : Event.Parameter) => (ep.name, ep.`type`, ep.internalType, ep.indexed) )
    implicit val Ordering_Function = Ordering.by( (fcn : Function) => ( fcn.name, fcn.inputs, fcn.outputs, fcn.constant, fcn.payable, fcn.stateMutability ) )
    implicit val Ordering_Constructor = Ordering.by( (ctor : Constructor) => ( ctor.inputs, ctor.payable, ctor.stateMutability ) )
    implicit val Ordering_Event = Ordering.by( (ev : Event) => ( ev.name, ev.inputs, ev.anonymous ) )

    def apply( abi : Abi ) : Abi = Abi( abi.functions.sorted, abi.events.sorted, abi.constructors.sorted, abi.receive, abi.fallback )
  }
}
final case class Abi(
  functions : immutable.Seq[Abi.Function],
  events : immutable.Seq[Abi.Event],
  constructors : immutable.Seq[Abi.Constructor],
  receive : Option[Abi.Receive],
  fallback : Option[Abi.Fallback]
) extends MaybeEmpty {
  def isEmpty : Boolean = functions.isEmpty && events.isEmpty && constructors.isEmpty && receive == null && fallback == null
  def withStandardSort : Abi = Abi.StandardSort.apply( this )
}

