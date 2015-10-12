package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import com.mchange.sc.v2.failable._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned16
import com.mchange.sc.v1.consuela.ethereum.encoding.{RLP,RLPSerializing};

import scala.collection.immutable;

object Payload {
  final object Factory {
    abstract class Base[P <: Payload[_]]( val subprotocol : Subprotocol )( implicit rlp : RLPSerializing[P] )  extends Payload.Factory[P] {
      def pickle( payload : P )         : immutable.Seq[Byte] = RLP.encode[P]( payload )
      def unpickle( bytes : Seq[Byte] ) : Failable[P]         = RLP.decodeComplete[P]( bytes );
    }
  }
  trait Factory[P <: Payload[_]] {
    def subprotocol                   : Subprotocol;

    def pickle( payload : P )         : immutable.Seq[Byte];
    def unpickle( bytes : Seq[Byte] ) : Failable[P];

    def attemptPickle( payload : Payload[_] ) : Failable[immutable.Seq[Byte]] = {
      if (payload.factory == this) {
        succeed( pickle( payload.asInstanceOf[P] ) ) 
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


