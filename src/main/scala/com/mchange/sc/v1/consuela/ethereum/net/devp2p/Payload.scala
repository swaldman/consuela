package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import com.mchange.sc.v2.failable._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned16
import com.mchange.sc.v1.consuela.ethereum.encoding.{RLP,RLPSerializing};

import scala.collection.immutable;

object Payload {
  final object Factory {
    abstract class Base[P <: Payload[P]]( val subprotocol : Subprotocol )( implicit val rlp : RLPSerializing[P] ) extends Payload.Factory[P];
  }
  trait Factory[P <: Payload[P]] {
    def subprotocol : Subprotocol;
    def rlp         : RLPSerializing[P];  

    def validate( payload : Payload[_] ) : Failable[P] = {
      if (payload.factory == this) {
        succeed( payload.asInstanceOf[P] )
      } else {
        fail( s"Cannot pickle, payload ${payload} is inappropriate for factory ${this}." )
      }
    }

    lazy val offset : Unsigned16 = Unsigned16( subprotocol.PayloadFactories.indexOf( this ) )
  }
  abstract class Base[P <: Payload[P]]( val factory : Payload.Factory[P] ) extends Payload[P];
}
trait Payload[P <: Payload[P]] {
  val factory  : Payload.Factory[P];
  def offset   : Unsigned16 = factory.offset // the subprotocol-defined offset for this type of Payload

  def typeCode : Unsigned16; // the Session-specific typeCode of this Payload object
}


