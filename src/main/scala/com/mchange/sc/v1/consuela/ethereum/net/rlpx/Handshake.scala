package com.mchange.sc.v1.consuela.ethereum.net.rlpx

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.restrict.RestrictedByteSeq;

import com.mchange.sc.v1.consuela.ethereum.ethcrypt.bouncycastle.EthECIES._

import com.mchange.sc.v2.failable._;

import scala.collection._

import java.security.SecureRandom

object Handshake {

  private def genKeyPair( random : SecureRandom ) : EthKeyPair = toEthKeyPair( generateEphemeralKeyPair( random ) )
  private def genNonce( random : SecureRandom ) : ByteSeqExact32 = ByteSeqExact32.assert( ImmutableArraySeq.Byte.random(32)( random ) )

  final object Block {
    private type MessageBuilder = (
      ByteSeqExact16,        // initialization vector
      EthKeyPair,            // an ephemeral key pair
      ByteSeqExact32,        // shared secret
      SecureRandom           // source of randomness
    ) => Message

    private def createBlock( random : SecureRandom, handshakeSharedSecret : ByteSeqExact32, recipientPublicKey : EthPublicKey )( buildMessage : MessageBuilder ) : (Block, Message) = {
      val initializationVector = ByteSeqExact16.assert( ImmutableArraySeq.Byte.random(16)( random ) )
      val ephemeralKeyPair = genKeyPair( random )
      val message = buildMessage( initializationVector, ephemeralKeyPair, handshakeSharedSecret, random )
      val plaintext = message.bytes.widen.toArray
      val ciphertextBlock = encryptBlock( ephemeralKeyPair.pvt, recipientPublicKey, initializationVector.widen.toArray, Some( handshakeSharedSecret.widen.toArray ), plaintext, 0, plaintext.length )
      val shortEncryptedBlock = EncryptedBlock.Short( ciphertextBlock )
      val block = Block( ephemeralKeyPair.pub, initializationVector, ImmutableArraySeq.Byte.createNoCopy( shortEncryptedBlock.ciphertext ), ByteSeqExact32( shortEncryptedBlock.mac ) )
      ( block, message )
    }
    def parse( bytes : Seq[Byte] ) : Failable[Block] = {
      val macIndex = bytes.length - MacBytes

      val pubKeyBytes     = bytes.slice( 0, 65 )
      val aivBytes        = bytes.slice( 65, 81 )
      val ciphertextBytes = bytes.slice( 81, macIndex )
      val mac             = bytes.drop( 81 )

      EthPublicKey.fromBytesWithUncompressedHeader( ByteSeqExact65.assert( pubKeyBytes ) ).map { ethPublicKey =>
        Block( ethPublicKey, ByteSeqExact16.assert( aivBytes ), ciphertextBytes.toImmutableSeq, ByteSeqExact32.assert( mac ) )
      }
    }
    /*
    final object Initiator {
      def create( recipientPublicKey : EthPublicKey, senderPublicKey : EthPublicKey, mbSharedSecret : Option[ByteSeqExact32] )( random : SecureRandom ) : (Block, Message.Initiator) = {
        val ( block, message ) = createBlock( random ) { ( initializationVector, ephemeralKeyPair, sharedSecret, randomness ) => 
          Message.Initiator.create( sharedSecret, mbSharedSecret != None, ephemeralKeyPair, senderPublicKey )( randomness )
        }
        ( block, message.asInstanceOf[Message.Initiator]
      }
    }
    final object Receiver {
      def create( peerIsKnown : Boolean, sharedSecret : ByteSeqExact32 ) : (Block, Message.Receiver) = {
        val ( block, message ) = createBlock( random ) { ( initializationVector, ephemeralKeyPair, sharedSecret, randomness ) => 
          Message.Initiator.create( sharedSecret, mbSharedSecret != None, ephemeralKeyPair, senderPublicKey )( randomness )
        }
        ( block, message.asInstanceOf[Message.Initiator]
      }
     */ 
      /*
      def create( initiator : Message.Initiator, mbKnownPeerDb : Option[KnownPeer.Database] )( random : SecureRandom ) : Message.Receiver = {
        val ( sharedSecret, isKnownPeer ) = {
          def autoSharedSecret = ???
          def unknownPeerTuple = ( autoSharedSecret, false )
          ( initiator.initiatorKnownPeer, mbKnownPeerDb ) match {
            case ( true, Some( knownPeerDb ) ) => {
              val mbKnownPeer = knownPeerDb.seek( initiator.permanentPublicKey ).logRecover( WARNING, _ => None ).get
              mbKnownPeer.fold( unknownPeerTuple )( knownPeer => (knownPeer.sessionToken, true) )
            }
            case _ => unknownPeerTuple
          }
        }
        val ( block, message ) = createBlock( mbSharedSecret, random ) { ( initializationVector, ephemeralKeyPair, sharedSecret, randomness ) =>
          Message.Receiver.create( isKnownPeer, ephemeralKeyPair )( randomness )
        }
        ( block, message.asInstanceOf[Message.Receiver]
      }
    }
    */
    def apply( bytes : Array[Byte] ) : Failable[Block] = {
      val pubKeyBytes = bytes.slice(  0, 65 )
      val aesIVBytes  = bytes.slice( 65, 81 )
      val shortEncryptedBlock = EncryptedBlock.Short( bytes.drop(81) )

      EthPublicKey.fromBytesWithUncompressedHeader( ByteSeqExact65 ( pubKeyBytes ) ).map{ ethPublicKey =>
        Block(
          ethPublicKey,
          ByteSeqExact16( aesIVBytes ),
          ImmutableArraySeq.Byte( shortEncryptedBlock.ciphertext ),
          ByteSeqExact32( shortEncryptedBlock.mac )
        )
      }
    }
  }
  case class Block( eciesPublicKey : EthPublicKey, aesInitialVector : ByteSeqExact16, ciphertext : immutable.Seq[Byte], eciesMac : ByteSeqExact32 ) {
    private lazy val _bytes = Array.concat( // treat as immutable, do not modify!
      eciesPublicKey.bytesWithUncompressedHeader.widen.toArray,
      aesInitialVector.widen.toArray,
      ciphertext.toArray,
      eciesMac.widen.toArray
    )
    lazy val bytes : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( _bytes )

    def decryptToPlaintext( to : EthPrivateKey, mbSharedSecret : Option[ByteSeqExact32] ) : Array[Byte] = {
      val sharedSecret = mbSharedSecret.getOrElse( ByteSeqExact32( ecdheSharedSecret( to, eciesPublicKey ) ) )
      val cipherMac = Array.concat( ciphertext.toArray, eciesMac.widen.toArray )
      decryptBlock( eciesPublicKey, to, aesInitialVector.widen.toArray, Some( sharedSecret.widen.toArray ), cipherMac, 0, cipherMac.length )
    }
    def decryptToInitiatorMessage( to : EthPrivateKey, mbSharedSecret : Option[ByteSeqExact32] ) : Failable[Message.Initiator] = {
      Message.Initiator( this.decryptToPlaintext( to, mbSharedSecret ) )
    }
    def decryptToReceiverMessage( to : EthPrivateKey, mbSharedSecret : Option[ByteSeqExact32] ) : Failable[Message.Receiver] = {
      Message.Receiver( this.decryptToPlaintext( to, mbSharedSecret ) )
    }
    def decryptToMessage( to : EthPrivateKey, mbSharedSecret : Option[ByteSeqExact32] ) : Failable[Message] = {
      val plaintext = this.decryptToPlaintext( to, mbSharedSecret )
      plaintext.length match {
        case Message.Initiator.Length => succeed( Message.Initiator._apply( this.decryptToPlaintext( to, mbSharedSecret ) ) )
        case Message.Receiver.Length  => succeed( Message.Receiver._apply( this.decryptToPlaintext( to, mbSharedSecret ) ) )
        case _                        => fail( s"Plaintext length ${plaintext.length} does not match the expected length of any Handshake.Message." )
      }
    }
  }
  final object Message {
    final object Initiator {
      val Length = 194
      def apply( arr : Array[Byte] ) : Failable[Initiator] = {
        if ( arr.length == Length ) succeed( _apply(arr) ) else fail( s"Initator must be ${Length} bytes, found ${arr.length} bytes" )
      }
      def apply( bytes : ByteSeqExact194 ) : Initiator = _apply( bytes.widen.toArray )

      private[Handshake] def _apply( arr : Array[Byte] ) : Initiator = {
        val sig   = EthSignature.fromBytesRSI( arr, 0 )     // 65 bytes, indices  0 thru 64
        val ekm   = EthHash.withBytes( arr, 65, 32 )        // 32 bytes, indices 65 thru 96
        val ppk   = EthPublicKey( arr, 97 )                 // 64 bytes, indices 97 thru 160
        val nonce = ByteSeqExact32( arr.slice( 161, 193 ) ) // 32 bytes, indices 161 thru 192
        val ikp   = Unsigned1( arr(193) )                   //  1 byte @ index 193
        Initiator( sig, ekm, ppk, nonce, ikp )
      }
      def create( sharedSecret : ByteSeqExact32, isKnownPeer : Boolean, ephemeralKeyPair : EthKeyPair, senderPermanentPublicKey : EthPublicKey )( implicit random : SecureRandom ) : Initiator = {
        val initiatorNonce = genNonce( random )
        val ephemeralKeyMac = EthHash.hash( ephemeralKeyPair.pub.bytes )
        val initiatorKnownPeer = if ( isKnownPeer ) Unsigned1.assert(1) else Unsigned1.assert(0)
        val signMe = (sharedSecret.widen ^ initiatorNonce.widen) // this 32 byte quantity is signed directly, without further hashing
        val signature = ephemeralKeyPair.pvt.signEthHash( EthHash.withBytes( signMe ) )
        Initiator( signature, ephemeralKeyMac, senderPermanentPublicKey, initiatorNonce, initiatorKnownPeer )
      }
    }
    final case class Initiator (
      signature          : EthSignature,
      ephemeralKeyMac    : EthHash, // hash of the 64-byte encoded public key (without the 0x04 "decompressed" header)
      permanentPublicKey : EthPublicKey,
      initiatorNonce     : ByteSeqExact32,
      initiatorKnownPeer : Unsigned1
    ) extends Message {
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
      val Length = 97
      def apply( arr : Array[Byte] ) : Failable[Receiver] = {
        if ( arr.length == Length ) succeed( _apply(arr) ) else fail( s"Receiver must be ${Length} bytes, found ${arr.length} bytes" )
      }
      def apply( bytes : ByteSeqExact97 ) : Receiver = _apply( bytes.widen.toArray )

      private[Handshake] def _apply( arr : Array[Byte] ) : Receiver = {
        val rerpk = EthPublicKey( arr, 0 )                 // 64 bytes, indices  0 thru 63
        val nonce = ByteSeqExact32( arr.slice( 64, 96 ) )  // 32 bytes, indices 64 thru 95
        val rkp   = Unsigned1( arr(96) )                   //  1 byte @ index 96
        Receiver( rerpk, nonce, rkp )
      }
      def create( isKnownPeer : Boolean, ephemeralKeyPair : EthKeyPair )( implicit random : SecureRandom ) : Receiver = {
        val receiverNonce = genNonce( random )
        val receiverKnownPeer = if ( isKnownPeer ) Unsigned1( 1 ) else Unsigned1( 0 )
        Receiver( ephemeralKeyPair.pub, receiverNonce, receiverKnownPeer )
      }
    }
    final case class Receiver (
      receiverEcdheRandomPublicKey : EthPublicKey,
      receiverNonce                : ByteSeqExact32,
      receiverKnownPeer            : Unsigned1
    ) extends Message {
      lazy val bytes : ByteSeqExact97 = {
        val buff = new mutable.ArrayBuffer[Byte]( 97 ) // serializes to exactly 97 bytes
        buff ++= receiverEcdheRandomPublicKey.toByteSeqExact64.widen
        buff ++= receiverNonce.widen
        buff +=  receiverKnownPeer.widen // just one Byte
        ByteSeqExact97.assert( ImmutableArraySeq.Byte( buff.toArray ) )
      }
    }
  }
  sealed trait Message {
    val bytes : RestrictedByteSeq.Shield;
  }
}
