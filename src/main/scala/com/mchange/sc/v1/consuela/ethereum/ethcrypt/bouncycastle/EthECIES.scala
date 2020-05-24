package com.mchange.sc.v1.consuela.ethereum.ethcrypt.bouncycastle;

import com.mchange.sc.v1.consuela._

import com.mchange.sc.v1.consuela.ethereum.{EthPrivateKey,EthPublicKey,EthKeyPair}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,ByteSeqExact64}

import java.security.SecureRandom
import java.util.Random

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.{BufferedBlockCipher,Digest,Mac}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.math.ec.ECCurve
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.modes.SICBlockCipher // implements CTR mode
import org.bouncycastle.crypto.params.{ECDomainParameters,ECKeyGenerationParameters,ECPrivateKeyParameters,ECPublicKeyParameters,KeyParameter,ParametersWithIV}
import org.bouncycastle.crypto.tls.NamedCurve

import scala.language.implicitConversions

/*
 * Everything about this is liberally stolen, um, adapted from 
 * bouncycastle and EthereumJ implementations!
 */ 
object EthECIES {
  val CipherKeyBits  = 128
  val CipherKeyBytes = CipherKeyBits / 8

  val MacKeyBits  = 128;
  val MacKeyBytes = MacKeyBits / 8

  val MacBits  = 256;
  val MacBytes = MacBits / 8

  val DigestBits  = 256
  val DigestBytes = DigestBits / 8 

  val KdfCounterStart = 1

  val CurveInfo = {
    val ECParamBundleName = "secp256k1"
    val NamedCurve        = SECNamedCurves.getByName(ECParamBundleName);
    val tmpCurveParams    = new ECDomainParameters(NamedCurve.getCurve(), NamedCurve.getG(), NamedCurve.getN(), NamedCurve.getH());
    (tmpCurveParams, tmpCurveParams.getCurve().asInstanceOf[ECCurve.Fp])
  }
  val CurveParams = CurveInfo._1
  val Curve       = CurveInfo._2

  val EncodedPublicKeyLen = 1 + 32 + 32; // 0x4 then two 256 bit values

  val EmptyByteArray = Array.ofDim[Byte](0)
  def DerivationVector = EmptyByteArray
  def EncodingVector   = EmptyByteArray

  // just an over-elaborate parser for the
  // results of this class' encryption
  final object EncryptedBlock {
    final object Short {
      def apply( bytes : Array[Byte] ) : EncryptedBlock.Short = {
        require ( bytes.length >= MacBytes )

        val macIndex = (bytes.length - MacBytes)
        var ciphertext       = bytes.slice( 0, macIndex )
        var mac              = bytes.slice( macIndex, bytes.length )

        new EncryptedBlock.Short( ciphertext, mac )
      }
    }
    class Short( ciphertext : Array[Byte], mac : Array[Byte] ) extends EncryptedBlock( ciphertext, mac );
    final object Long {
      def apply( bytes : Array[Byte] ) : EncryptedBlock.Long = {
        require ( bytes.length >= EncodedPublicKeyLen + MacBytes )

        val macIndex = (bytes.length - MacBytes)
        var encodedPublicKey = bytes.slice( 0, EncodedPublicKeyLen )
        var ciphertext       = bytes.slice( EncodedPublicKeyLen, macIndex )
        var mac              = bytes.slice( macIndex, bytes.length )

        new EncryptedBlock.Long( encodedPublicKey, ciphertext, mac )
      }
    }
    class Long( var encodedPublicKey : Array[Byte], ciphertext : Array[Byte], mac : Array[Byte] ) extends EncryptedBlock( ciphertext, mac );
  }
  sealed abstract class EncryptedBlock( var ciphertext : Array[Byte], var mac : Array[Byte] )

  def createCipher( encrypt : Boolean, key : Array[Byte], initializationVector : Array[Byte] ) : BufferedBlockCipher = {
    val out = new BufferedBlockCipher( new SICBlockCipher( new AESEngine ) )
    out.init( encrypt, new ParametersWithIV( new KeyParameter( key ), initializationVector ) )
    out
  }

  def createDigest() : Digest = new SHA256Digest

  def createMac() : Mac = new HMac( createDigest() )

  def createRandom() : SecureRandom = new java.security.SecureRandom

  def generateInitializationVector( random : SecureRandom = createRandom() ) : Array[Byte] = {
    val out = Array.ofDim[Byte]( CipherKeyBytes )
    random.nextBytes( out )
    out
  }

  def generateEphemeralKeyPair( random : SecureRandom = createRandom() ) : ( ECPrivateKeyParameters, ECPublicKeyParameters ) = {
    val gen = new ECKeyPairGenerator()
    val genParams = new ECKeyGenerationParameters(CurveParams, random)
    gen.init( genParams )

    val keyPair = gen.generateKeyPair()
    ( keyPair.getPrivate.asInstanceOf[ECPrivateKeyParameters], keyPair.getPublic.asInstanceOf[ECPublicKeyParameters] )
  }

  // adapted from EthereumJ's org.ethereum.ConcatKDFBytesGenerator
  def kdf( sharedSecret : Array[Byte], initializationVector : Array[Byte], len : Int ) : Array[Byte] = {
    val digest = createDigest()

    val oBytes = len.toLong
    val outLen = DigestBytes

    // omitted test of the size of oBytes, because AFICT, there's no way
    // what starts as an Int, even if interpreted as unsigned, could exceed 
    // the given max value

    val cThreshold = ((oBytes + outLen - 1) / outLen).toInt

    val dig = Array.ofDim[Byte]( outLen ) // initialized to zeroes

    val C = KdfCounterStart.toByteArrayBigEndian

    val out = Array.ofDim[Byte]( len )

    var outOffset = 0
    var counterBase = (KdfCounterStart & ~0xFF)
    var remaining = len

    (0 until cThreshold).foreach { i =>
      digest.update(C, 0, C.length )
      digest.update( sharedSecret, 0, sharedSecret.length )
      if ( initializationVector != null ) digest.update( initializationVector, 0, initializationVector.length )

      digest.doFinal( dig, 0 ) // fills dig with the newly computed digest

      if ( remaining > outLen ) {
        Array.copy( dig, 0, out, outOffset, outLen )
        outOffset += outLen
        remaining -= outLen
      } else {
        Array.copy( dig, 0, out, outOffset, outLen )
      }

      C(3) = (C(3) + 1).toByte
      if (C(3)==0) {
        counterBase += 0x100
        counterBase.fillBigEndian( C )
      }
    }

    // we don't bother resetting the digest, since we throw it away
    // we don't return oBytes, just our key

    out
  }

  // adapted from EthereumJ's org.ethereum.crypto.EthereumIESEngine
  // sharedSecret is VZ in that class
  def doEncryptBlock( 
    mbEmbeddedEncodedPublicKey : Option[Array[Byte]],
    sharedSecret               : Array[Byte], 
    derivationVector           : Array[Byte],
    encodingVector             : Array[Byte],
    initializationVector       : Array[Byte],
    in                         : Array[Byte], 
    inOffset                   : Int, 
    inLen                      : Int
  ) : Array[Byte] = {
    val V = mbEmbeddedEncodedPublicKey.getOrElse( EmptyByteArray )

    val K  = kdf( sharedSecret, derivationVector, CipherKeyBytes + MacKeyBytes )
    val Ksplit = K.splitAt( CipherKeyBytes )
    val K1 = Ksplit._1
    val K2 = Ksplit._2

    val P2 = encodingVector
    val L2 = {
      val l2int = if ( V.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( true, K1, initializationVector )

    val C = Array.ofDim[Byte]( cipher.getOutputSize( inLen ) )
    var len = cipher.processBytes( in, inOffset, inLen, C, 0 )
    len += cipher.doFinal( C, len )

    val K2a = Array.ofDim[Byte]( DigestBytes )
    val hash = createDigest()
    hash.update(K2, 0, K2.length);
    hash.doFinal(K2a, 0);

    val T = {
      val t = Array.ofDim[Byte]( MacBytes )
      val mac = createMac()
      mac.init( new KeyParameter( K2a ) )
      mac.update( initializationVector, 0, initializationVector.length )
      mac.update( C, 0, C.length )
      if (P2 != null) mac.update(P2, 0, P2.length);
      if (V.length != 0) mac.update(L2, 0, L2.length); // this just seems wrong somehow
      mac.doFinal( t, 0 )
      t
    }

    Array.concat( V, C, T )
  }

  // adapted from EthereumJ's org.ethereum.crypto.EthereumIESEngine
  // sharedSecret is VZ in that class
  def doDecryptBlock( 
    mbEmbeddedEncodedPublicKey : Option[Array[Byte]],
    sharedSecret               : Array[Byte], 
    derivationVector           : Array[Byte], 
    encodingVector             : Array[Byte], 
    initializationVector       : Array[Byte], 
    in                         : Array[Byte], 
    inOffset                   : Int, 
    inLen                      : Int 
  ) : Array[Byte] = {
    require( inLen > MacKeyBytes )

    val V = mbEmbeddedEncodedPublicKey.getOrElse( EmptyByteArray )

    val K  = kdf( sharedSecret, derivationVector, CipherKeyBytes + MacKeyBytes )
    val Ksplit = K.splitAt( CipherKeyBytes )
    val K1 = Ksplit._1
    val K2 = Ksplit._2

    val P2 = encodingVector
    val L2 = {
      val l2int = if ( V.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( false, K1, initializationVector )

    val M = Array.ofDim[Byte]( cipher.getOutputSize(inLen - V.length - MacBytes) ) // will be PLAINTEXT ONLY
    var len = cipher.processBytes(in, inOffset + V.length, inLen - V.length - MacBytes, M, 0)
    len += cipher.doFinal(M, len)

    // Verify MAC
    val inEnd = inOffset + inLen
    val T1 = in.slice( inEnd - MacBytes, inEnd )

    val K2a = {
      val k2a = Array.ofDim[Byte]( DigestBytes )
      val hash = createDigest()
      hash.update(K2, 0, K2.length);
      hash.doFinal(k2a, 0);
      k2a
    }

    val T2  = {
      val t2 = Array.ofDim[Byte]( T1.length )
      val mac = createMac()
      mac.init(new KeyParameter(K2a));
      mac.update(initializationVector, 0, initializationVector.length);
      mac.update(in, inOffset + V.length, inLen - V.length - t2.length);
      if (P2 != null) mac.update(P2, 0, P2.length);
      if (V.length != 0) mac.update(L2, 0, L2.length);
      mac.doFinal(t2, 0);
      t2
    }

    if (T1.sameElements(T2)) {
      M.slice(0, len) // Decoded plaintext
    } else {
      throw new Exception( s"MACs don't agree. T1: ${T1.hex}, T2: ${T2.hex}" )
    }
  }

  // adapted from org.bouncycastle.crypto.agreement.ECDHBasicAgreement
  def ecdheSharedSecret( myPriv : ECPrivateKeyParameters, yourPub : ECPublicKeyParameters ) : Array[Byte] = {
    val P = yourPub.getQ().multiply(myPriv.getD()).normalize();

    if (P.isInfinity()) throw new IllegalStateException("Infinity is not a valid agreement value for ECDH");

    val fieldSize = (myPriv.getParameters().getCurve().getFieldSize() + 7) / 8;

    return P.getAffineXCoord().toBigInteger().unsignedBytes( fieldSize )
  }

  def ecdheSharedSecret( myPriv : EthPrivateKey, yourPub : EthPublicKey ) : Array[Byte] = ecdheSharedSecret( toECPrivateKeyParameters( myPriv ), toECPublicKeyParameters( yourPub ) )

  def readEncodedPublicKey( keyBytes : Array[Byte] ) : ECPublicKeyParameters = {
    val point = Curve.decodePoint(keyBytes)
    new ECPublicKeyParameters( point, CurveParams )
  }

  def encodePublicKey( pub : ECPublicKeyParameters ) : Array[Byte] = pub.getQ.getEncoded(false)

  private def valueAsArray( bi : BigInt ) = bi.unsignedBytes( crypto.secp256k1.ValueByteLength )
  
  implicit def toEthPublicKey( pub : ECPublicKeyParameters ) : EthPublicKey = {
    val point = pub.getQ.normalize
    EthPublicKey( ByteSeqExact64( Array.concat( point.getXCoord.getEncoded, point.getYCoord.getEncoded ) ) )
  }

  implicit def toECPublicKeyParameters( ethPub : EthPublicKey ) : ECPublicKeyParameters = new ECPublicKeyParameters( Curve.createPoint( ethPub.x.bigInteger, ethPub.y.bigInteger ), CurveParams )

  implicit def toEthPrivateKey( priv : ECPrivateKeyParameters ) : EthPrivateKey = EthPrivateKey( ByteSeqExact32( valueAsArray( priv.getD ) ) )

  implicit def toECPrivateKeyParameters( ethPriv : EthPrivateKey ) : ECPrivateKeyParameters = new ECPrivateKeyParameters( ethPriv.s.bigInteger, CurveParams )

  def toEthKeyPair( rawKeyPair  : (ECPrivateKeyParameters, ECPublicKeyParameters) ) : EthKeyPair = {
    EthKeyPair( toEthPrivateKey( rawKeyPair._1 ), toEthPublicKey( rawKeyPair._2 ) )
  }

  /**
    * Returns the concatenation of ciphertext and MAC in a byte array.
    */ 
  def encryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPublicKeyParameters,
    initializationVector : Array[Byte],
    mbSharedSecret : Option[Array[Byte]],
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = mbSharedSecret.getOrElse( ecdheSharedSecret( from, to ) )
    doEncryptBlock( None, sharedSecret, DerivationVector, EncodingVector, initializationVector, bytes, offset, len )
  }

  /**
    * Returns the concatenation of an ephemeral sender public key, ciphertext and MAC in a byte array.
    */ 
  def encryptBlock(
    to : ECPublicKeyParameters,
    initializationVector : Array[Byte], 
    mbSharedSecret : Option[Array[Byte]],
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val from = generateEphemeralKeyPair()
    val sharedSecret = mbSharedSecret.getOrElse( ecdheSharedSecret( from._1, to ) )
    doEncryptBlock( Some( encodePublicKey( from._2 ) ), sharedSecret, DerivationVector, EncodingVector, initializationVector, bytes, offset, len )
  }

  /**
    * Requires the sender's known public key.
    *  
    * Accepts the concatenation of ciphertext and MAC in a byte array.
    */ 
  def decryptBlock(
    from : ECPublicKeyParameters,
    to : ECPrivateKeyParameters, 
    initializationVector : Array[Byte], 
    mbSharedSecret : Option[Array[Byte]],
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = mbSharedSecret.getOrElse( ecdheSharedSecret( to, from ) )
    doDecryptBlock( None, sharedSecret, DerivationVector, EncodingVector, initializationVector, bytes, offset, len )
  }

  /**
    * Accepts the concatenation of an ephemeral public sender key, ciphertext and MAC in a byte array.
    */ 
  def decryptBlock(
    to : ECPrivateKeyParameters, 
    initializationVector : Array[Byte], 
    mbSharedSecret : Option[Array[Byte]],
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val fromKeyBytes = bytes.slice( offset, offset + EncodedPublicKeyLen )
    val from = readEncodedPublicKey( fromKeyBytes )
    val sharedSecret = mbSharedSecret.getOrElse( ecdheSharedSecret( to, from ) )
    doDecryptBlock( Some( fromKeyBytes ), sharedSecret, DerivationVector, EncodingVector, initializationVector, bytes, offset, len )
  }
}

