package com.mchange.sc.v1.consuela.ethereum.util;

import com.mchange.sc.v1.consuela.ethereum.{Nibble, toNibbles => nibbles};
import com.mchange.sc.v1.consuela.util.ByteArrayValue;

object EthByteArrayValue {
  trait Nibbly {
    self : ByteArrayValue =>

    lazy val toNibbles : IndexedSeq[Nibble] = nibbles( this.bytes );
  }
}
