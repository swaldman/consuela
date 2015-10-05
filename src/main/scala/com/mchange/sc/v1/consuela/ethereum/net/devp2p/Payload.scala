package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import com.mchange.sc.v2.failable._

trait Payload {
  def offset   : Int; // the subprotocol-defined offset for this type of Payload
  def typeCode : Int; // the Session-specific typeCode of this Payload object
}


