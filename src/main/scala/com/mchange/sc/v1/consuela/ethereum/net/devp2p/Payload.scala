package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import com.mchange.sc.v2.failable._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned16

trait Payload {
  def offset   : Unsigned16; // the subprotocol-defined offset for this type of Payload
  def typeCode : Unsigned16; // the Session-specific typeCode of this Payload object
}


