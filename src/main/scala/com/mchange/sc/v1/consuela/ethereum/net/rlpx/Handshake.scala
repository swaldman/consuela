package com.mchange.sc.v1.consuela.ethereum.net.rlpx;

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import scala.collection.mutable;

object Handshake {
  final object Message {
    final object Initiator {
      def apply( arr : Array[Byte] ) : Initiator = {
        val sig   = EthSignature.fromBytesRSV( arr, 0 )     // 65 bytes, indices  0 thru 64
        val ekm   = EthHash.withBytes( arr, 65, 32 )        // 32 bytes, indices 65 thru 96
        val ppk   = EthPublicKey( arr, 97 )                 // 64 bytes, indices 97 thru 160 
        val nonce = ByteSeqExact32( arr.slice( 161, 193 ) ) // 32 bytes, indices 161 thru 192
        val ikp   = Unsigned1( arr(193) )                   //  1 byte @ index 193
        Initiator( sig, ekm, ppk, nonce, ikp )
      }
    }
    final case class Initiator (
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
        buff ++= permanentPublicKey.toByteSeqExact64.widen
        buff ++= initiatorNonce.widen
        buff += initiatorKnownPeer.widen // just one Byte
        ByteSeqExact194.assert( ImmutableArraySeq.Byte( buff.toArray ) )
      }
    }
    final object Receiver {
      def apply( arr : Array[Byte] ) : Receiver = {
        val rerpk = EthPublicKey( arr, 0 )                 // 64 bytes, indices  0 thru 63
        val nonce = ByteSeqExact32( arr.slice( 64, 96 ) )  // 32 bytes, indices 64 thru 95
        val rkp   = Unsigned1( arr(96) )                   //  1 byte @ index 96
        Receiver( rerpk, nonce, rkp )
      }
    }
    final case class Receiver (
      receiverEcdheRandomPublicKey : EthPublicKey,
      receiverNonce                : ByteSeqExact32,
      receiverKnownPeer            : Unsigned1
    ) {
      lazy val bytes : ByteSeqExact97 = {
        val buff = new mutable.ArrayBuffer[Byte]( 97 ) // serializes to exactly 97 bytes
        buff ++= receiverEcdheRandomPublicKey.toByteSeqExact64.widen
        buff ++= receiverNonce.widen
        buff +=  receiverKnownPeer.widen // just one Byte
        ByteSeqExact97.assert( ImmutableArraySeq.Byte( buff.toArray ) )
      }
    }
  }
}
