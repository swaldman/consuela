package com.mchange.sc.v1.consuela.ethereum.net.rlpx

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v1.consuela.ethereum.ethcrypt.bouncycastle.EthECIES._

import scala.collection.mutable

import java.security.SecureRandom

object Handshake {
  final object Message {
    private def genKeyPair( random : SecureRandom ) : EthKeyPair = toEthKeyPair( generateEphemeralKeyPair( random ) )
    private def genNonce( random : SecureRandom ) : ByteSeqExact32 = {
      val tmp = Array.ofDim[Byte]( 32 )
      random.nextBytes( tmp )
      ByteSeqExact32.assert( tmp )
    }
    final object Initiator {
      def apply( arr : Array[Byte] ) : Initiator = {
        val sig   = EthSignature.fromBytesRSI( arr, 0 )     // 65 bytes, indices  0 thru 64
        val ekm   = EthHash.withBytes( arr, 65, 32 )        // 32 bytes, indices 65 thru 96
        val ppk   = EthPublicKey( arr, 97 )                 // 64 bytes, indices 97 thru 160 
        val nonce = ByteSeqExact32( arr.slice( 161, 193 ) ) // 32 bytes, indices 161 thru 192
        val ikp   = Unsigned1( arr(193) )                   //  1 byte @ index 193
        Initiator( sig, ekm, ppk, nonce, ikp )
      }
      def create( mbSharedSecret : Option[ByteSeqExact32], permanentPublicKey : EthPublicKey )( implicit random : SecureRandom ) : ( Initiator, EthKeyPair ) = {
        val ephemeralKeyPair = genKeyPair( random )
        val initiatorNonce = genNonce( random )
        val ephemeralKeyMac = EthHash.hash( ephemeralKeyPair.Public.bytes )
        val initiatorKnownPeer = mbSharedSecret.fold( Unsigned1.assert(0) )( _ => Unsigned1.assert(1) )
        val sharedSecret = mbSharedSecret.getOrElse( ByteSeqExact32( ecdheSharedSecret( ephemeralKeyPair.Private, permanentPublicKey ) ) )
        val signMe = (sharedSecret.widen ^ initiatorNonce.widen) // this 32 byte quantity is signed directly, without further hashing
        val signature = ephemeralKeyPair.Private.signEthHash( EthHash.withBytes( signMe ) )
        ( Initiator( signature, ephemeralKeyMac, permanentPublicKey, initiatorNonce, initiatorKnownPeer ), ephemeralKeyPair )
      }
    }
    final case class Initiator (
      signature          : EthSignature,
      ephemeralKeyMac    : EthHash, // hash of the 64-byte encoded public key (without the 0x04 "decompressed" header)
      permanentPublicKey : EthPublicKey,
      initiatorNonce     : ByteSeqExact32,
      initiatorKnownPeer : Unsigned1
    ) {
      lazy val bytes : ByteSeqExact194 = {
        val buff = new mutable.ArrayBuffer[Byte]( 194 ) // serializes to exactly 194 bytes
        buff ++= signature.exportBytesRSI.widen // note that this is in r ++ s ++ recId format
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
      def create( knownPeer : Boolean )( implicit random : SecureRandom ) : ( Receiver, EthKeyPair ) = {
        val ephemeralKeyPair = genKeyPair( random )
        val receiverNonce = genNonce( random )
        val receiverKnownPeer = if ( knownPeer ) Unsigned1( 1 ) else Unsigned1( 0 )
        ( Receiver( ephemeralKeyPair.Public, receiverNonce, receiverKnownPeer ), ephemeralKeyPair )
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
