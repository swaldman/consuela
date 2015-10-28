package com.mchange.sc.v1.consuela.ethereum.ethcrypt.bouncycastle;

import com.mchange.sc.v1.consuela._

import java.security.SecureRandom
import java.util.Random

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.{BufferedBlockCipher,Digest,Mac}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.math.ec.ECCurve
import org.bouncycastle.crypto.engines.AESFastEngine
import org.bouncycastle.crypto.generators.ECKeyPairGenerator
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.modes.SICBlockCipher // implements CTR mode
import org.bouncycastle.crypto.params.{ECDomainParameters,ECKeyGenerationParameters,ECPrivateKeyParameters,ECPublicKeyParameters,KeyParameter,ParametersWithIV}
import org.bouncycastle.crypto.tls.NamedCurve

/*
 * Everything about this is liberally stolen, um, adapted from 
 * bouncycastle and EthereumJ implementations!
 */ 
object EthECIES {
  val CipherKeyBits  = 128
  val CipherKeyBytes = CipherKeyBits / 8

  val MacKeyBits  = 128;
  val MacKeyBytes = MacKeyBits / 8

  val KdfCounterStart = 1

  val CurveInfo = {
    val ECParamBundleName = "secp256k1"
    val NamedCurve        = SECNamedCurves.getByName(ECParamBundleName);
    val tmpCurveParams    = new ECDomainParameters(NamedCurve.getCurve(), NamedCurve.getG(), NamedCurve.getN(), NamedCurve.getH());
    (tmpCurveParams, tmpCurveParams.getCurve().asInstanceOf[ECCurve.Fp])
  }
  val CurveParams = CurveInfo._1
  val Curve       = CurveInfo._2

  val EmptyByteArray = Array.ofDim[Byte](0)
  def getDerivationV = EmptyByteArray
  def getEncodingV   = EmptyByteArray

  def createCipher( encrypt : Boolean, key : Array[Byte], initializationVector : Array[Byte] ) : BufferedBlockCipher = {
    val out = new BufferedBlockCipher( new SICBlockCipher( new AESFastEngine ) )
    out.init( encrypt, new ParametersWithIV( new KeyParameter( key ), initializationVector ) )
    out
  }

  def createDigest() : Digest = new SHA256Digest

  def createMac() : Mac = new HMac( createDigest() )

  def createRandom() : SecureRandom = new java.security.SecureRandom

  def generateInitiationVector( random : SecureRandom = createRandom() ) : Array[Byte] = {
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
    val outLen = digest.getDigestSize()

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

  def kdf( sharedSecret : Array[Byte], len : Int ) : Array[Byte] = kdf( sharedSecret, getDerivationV, len )


  // adapted from EthereumJ's org.ethereum.crypto.EthereumIESEngine
  // sharedSecret is VZ in that class
  def doEncryptBlock( 
    encodedPublicKey     : Array[Byte],
    sharedSecret         : Array[Byte], 
    initializationVector : Array[Byte], 
    in                   : Array[Byte], 
    inOffset             : Int, 
    inLen                : Int 
  ) : Array[Byte] = {
    val K  = kdf( sharedSecret, CipherKeyBytes + MacKeyBytes )
    val Ksplit = K.splitAt( CipherKeyBytes )
    val K1 = Ksplit._1
    val K2 = Ksplit._2

    val P2 = getEncodingV
    val L2 = {
      val l2int = if ( encodedPublicKey.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( true, K1, initializationVector )

    val C = Array.ofDim[Byte]( cipher.getOutputSize( inLen ) )
    var len = cipher.processBytes( in, inOffset, inLen, C, 0 )
    len += cipher.doFinal( C, len )

    val hash = createDigest()
    val K2a = Array.ofDim[Byte]( hash.getDigestSize )
    hash.update(K2, 0, K2.length);
    hash.doFinal(K2a, 0);

    val T = {
      val mac = createMac()
      val t = Array.ofDim[Byte]( mac.getMacSize )
      mac.init( new KeyParameter( K2a ) )
      mac.update( initializationVector, 0, initializationVector.length )
      mac.update( C, 0, C.length )
      if (P2 != null) mac.update(P2, 0, P2.length);
      if (encodedPublicKey.length != 0) mac.update(L2, 0, L2.length); // this just seems wrong somehow
      mac.doFinal( t, 0 )
      t
    }

    Array.concat( encodedPublicKey, C, T )
  }

  // adapted from EthereumJ's org.ethereum.crypto.EthereumIESEngine
  // sharedSecret is VZ in that class
  def doDecryptBlock( 
    encodedPublicKey     : Array[Byte],
    sharedSecret         : Array[Byte], 
    initializationVector : Array[Byte], 
    in                   : Array[Byte], 
    inOffset             : Int, 
    inLen                : Int 
  ) : Array[Byte] = {
    require( inLen > MacKeyBytes )

    val K  = kdf( sharedSecret, CipherKeyBytes + MacKeyBytes )
    val Ksplit = K.splitAt( CipherKeyBytes )
    val K1 = Ksplit._1
    val K2 = Ksplit._2

    val P2 = getEncodingV
    val L2 = {
      val l2int = if ( encodedPublicKey.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( false, K1, initializationVector )
    val mac = createMac()

    val M = Array.ofDim[Byte]( cipher.getOutputSize(inLen - encodedPublicKey.length - mac.getMacSize()) ) // will be PLAINTEXT ONLY
    var len = cipher.processBytes(in, inOffset + encodedPublicKey.length, inLen - encodedPublicKey.length - mac.getMacSize(), M, 0)
    len += cipher.doFinal(M, len)

    // Verify MAC
    val inEnd = inOffset + inLen
    val T1 = in.slice( inEnd - mac.getMacSize, inEnd )

    val hash = createDigest()
    val K2a = Array.ofDim[Byte]( hash.getDigestSize )
    hash.update(K2, 0, K2.length);
    hash.doFinal(K2a, 0);

    val T2  = {
      val t2 = Array.ofDim[Byte]( T1.length )
      mac.init(new KeyParameter(K2a));
      mac.update(initializationVector, 0, initializationVector.length);
      mac.update(in, inOffset + encodedPublicKey.length, inLen - encodedPublicKey.length - t2.length);
      if (P2 != null) mac.update(P2, 0, P2.length);
      if (encodedPublicKey.length != 0) mac.update(L2, 0, L2.length);
      mac.doFinal(t2, 0);
      t2
    }

    if (T1 == T2) {
      M.slice(0, len) // Decoded plaintext
    } else {
      throw new Exception( s"MACs don't agree. T1: ${T1.hex}, T2: ${T2.hex}" )
    }
  }

  // adapted from org.bouncycastle.crypto.agreement.ECDHBasicAgreement
  def calculateSharedSecret( myPriv : ECPrivateKeyParameters, yourPub : ECPublicKeyParameters ) : Array[Byte] = {
    val P = yourPub.getQ().multiply(myPriv.getD()).normalize();

    if (P.isInfinity()) throw new IllegalStateException("Infinity is not a valid agreement value for ECDH");

    val fieldSize = (myPriv.getParameters().getCurve().getFieldSize() + 7) / 8;

    return P.getAffineXCoord().toBigInteger().unsignedBytes( fieldSize )
  }

  def readEncodedPublicKey( keyBytes : Array[Byte] ) : ECPublicKeyParameters = {
    val point = Curve.decodePoint(keyBytes)
    new ECPublicKeyParameters( point, CurveParams )
  }

  def encodePublicKey( pub : ECPublicKeyParameters ) : Array[Byte] = pub.getQ.getEncoded(false)

  private def encryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPublicKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = calculateSharedSecret( from, to )
    doEncryptBlock( encodedPublicKeyTo, sharedSecret, initializationVector, bytes, offset, len )
  }

  def encryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPublicKeyParameters,
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = encryptBlock( from, to, encodePublicKey( to ), initializationVector, bytes, offset, len )

  def encryptBlock(
    from : ECPrivateKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = encryptBlock( from, readEncodedPublicKey( encodedPublicKeyTo ), encodedPublicKeyTo, initializationVector, bytes, offset, len )

  private def decryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPublicKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = calculateSharedSecret( from, to )
    doDecryptBlock( encodedPublicKeyTo, sharedSecret, initializationVector, bytes, offset, len )
  }

  def decryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPublicKeyParameters,
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = decryptBlock( from, to, encodePublicKey( to ), initializationVector, bytes, offset, len )

  def decryptBlock(
    from : ECPrivateKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = decryptBlock( from, readEncodedPublicKey( encodedPublicKeyTo ), encodedPublicKeyTo, initializationVector, bytes, offset, len )
}

