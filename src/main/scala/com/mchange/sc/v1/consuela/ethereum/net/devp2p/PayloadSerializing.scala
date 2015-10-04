package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import com.mchange.sc.v1.consuela.ethereum.encoding.RLPSerializing;

abstract class PayloadSerializing[T <: Payload[T]]( val subprotocolOffset : Int )( val subprotocolStart : Int ) extends RLPSerializing[T] {
  lazy val sessionDependentType = subprotocolStart + subprotocolOffset;
}
