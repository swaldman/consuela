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
    initiatorNonce     : ByteSeqExact32,
    initiatorKnownPeer : Unsigned1
  ) {
    lazy val bytes : ByteSeqExact194 = {
      val buff = new mutable.ArrayBuffer[Byte]( 194 ) // serializes to exactly 194 bytes
      buff ++= signature.exportBytesRSV.widen
      buff ++= ephemeralKeyMac.bytes
      buff ++= permanentPublicKey.toByteSeqExact65.widen
      buff ++= initiatorNonce.widen
      buff += initiatorKnownPeer.widen // just one Byte
      ByteSeqExact194.assert( ImmutableArraySeq.Byte( buff.toArray ) )
    }
  }

  case class ReceiverHandshakeMessage (
    receiverEcdheRandomPubkey : EthPublicKey,
    receiverNonce             : ByteSeqExact32,
    receiverKnownPeer         : Unsigned1
  ) {
    lazy val bytes : ByteSeqExact97 = {
      val buff = new mutable.ArrayBuffer[Byte]( 97 ) // serializes to exactly 97 bytes
      buff ++= receiverEcdheRandomPubkey.toByteSeqExact64.widen
      buff ++= receiverNonce.widen
      buff +=  receiverKnownPeer.widen // just one Byte
      ByteSeqExact97.assert( ImmutableArraySeq.Byte( buff.toArray ) )
    }
  }

}
    
