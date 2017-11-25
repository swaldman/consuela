package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import scala.collection._
import play.api.libs.json._

final object Abi {
  def apply( json : String ) : Abi = Json.parse( json ).as[Abi]
  val empty = Abi( immutable.Seq.empty, immutable.Seq.empty, immutable.Seq.empty, None )

  final object Function {
    case class Parameter( name : String, `type` : String ) extends Abi.Parameter
  }
  final case class Function( name : String, inputs : immutable.Seq[Function.Parameter], outputs : immutable.Seq[Function.Parameter], constant : Boolean, payable : Boolean, stateMutability : String )

  final object Constructor {
    val noArgNoEffect = Constructor( Nil, false, "pure" )
    case class Parameter( name : String, `type` : String ) extends Abi.Parameter
  }
  final case class Constructor( inputs : immutable.Seq[Function.Parameter], payable : Boolean, stateMutability : String )

  final object Event {
    final case class Parameter( name : String, `type` : String, indexed : Boolean ) extends Abi.Parameter
  }
  final case class Event( name : String, inputs : immutable.Seq[Event.Parameter], anonymous : Boolean )

  final case class Fallback( payable : Boolean, stateMutability : String ) {
    def this( payable : Boolean ) = this( payable, if (payable) "payable" else "nonpayable" )
  }

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

