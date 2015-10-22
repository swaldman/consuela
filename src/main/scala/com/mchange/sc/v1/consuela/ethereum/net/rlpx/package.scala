package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import scala.collection.mutable;

package object rlpx {

  case class InitiatorHandshakeMessage (
    signature          : EthSignature,
    ephemeralKeyMac    : EthHash,
    permanentPublicKey : EthPublicKey,
    nonce              : ByteSeqExact32,
    knownPeer          : Unsigned1
  ) {
    lazy val bytes : ByteSeqExact194 = {
      val buff = new mutable.ArrayBuffer[Byte]( 194 )
      buff ++= signature.exportBytesRSV.widen
      buff ++= ephemeralKeyMac.bytes
      buff ++= permanentPublicKey.toByteSeqExact65.widen
      buff ++= nonce.widen
      buff += knownPeer.widen // just one Byte
      ByteSeqExact194.assert( ImmutableArraySeq.Byte( buff.toArray ) )
    }
  }

}
    
