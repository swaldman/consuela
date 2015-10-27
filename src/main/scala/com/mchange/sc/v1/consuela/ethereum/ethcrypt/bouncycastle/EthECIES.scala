package com.mchange.sc.v1.consuela.ethereum.ethcrypt.bouncycastle;

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

  val (CurveParams, Curve) = {
    val ECParamBundleName = "secp256k1"
    val NamedCurve        = SECNamedCurves.getByName(ECParamBundleName);
    val CurveParams       = new ECDomainParameters(NamedCurve.getCurve(), NamedCurve.getG(), NamedCurve.getN(), NamedCurve.getH());
    (CurveParams, CurveParams.getCurve().asInstanceOf[ECCurve.Fp])
  }

  val EmptyByteArray = Array.ofDim[Byte](0)
  def getDerivationV = EmptyByteArray
  def getEncodingV   = EmptyByteArray

  def createCipher( encrypt : Boolean, key : Array[Byte], initializationVector : Array[Byte] ) : BufferedBlockCipher = {
    val out = new BufferedBlockCipher( new SICBlockCipher( new AESFastEngine ) )
    out.init( encrypt, new ParametersWithIV( new KeyParameter( key ), initializatioVector ) )
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

      C(3) += 1
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
    val (K1, K2) = K.splitAt( CipherKeyBytes )

    val P2 = getEncodingV
    val L2 = {
      val l2int = if ( encodedPublicKey.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( true, K1, initializationVector )

    val C = Array.ofDim[Byte]( cipher.getOutputSize( inLen ) )
    var len = cipher.processBytes( in, inOffset, inLen, C, 0 )
    len += cipher.doFinal( C, len )

    val K2a = Array.ofDim[Byte]( hash.getDigestSize )
    val hash = createDigest()
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
    val (K1, K2) = K.splitAt( CipherKeyBytes )

    val P2 = getEncodingV
    val L2 = {
      val l2int = if ( encodedPublicKey.length != 0 && P2 != null ) (P2.length * 8) else 0
      l2int.toByteArrayBigEndian 
    }

    val cipher = createCipher( false, K1, initializationVector )

    val M = Array.ofDim[Byte]( cipher.getOutputSize(inLen - encodedPublicKey.length - mac.getMacSize()) ) // will be PLAINTEXT ONLY
    var len = cipher.processBytes(in_enc, inOff + encodedPublicKey.length, inLen - encodedPublicKey.length - mac.getMacSize(), M, 0)
    len += cipher.doFinal(M, len)

    // Verify MAC
    val inEnd = inOff + inLen
    val T1 = in.slice( inEnd - mac.getMacSize, end )

    val K2a = Array[Byte].ofDim( hash.getDigestSize )

    val hash = createDigest()
    hash.update(K2, 0, K2.length);
    hash.doFinal(K2a, 0);

    val T2  = {
      val t2 = Array.ofDim[Byte]( T1.length )
      mac = createMac()
      mac.init(new KeyParameter(K2a));
      mac.update(IV, 0, IV.length);
      mac.update(in, inOff + encodedPublicKey.length, inLen - encodedPublicKey.length - T2.length);
      if (P2 != null) mac.update(P2, 0, P2.length);
      if (encodedPublicKey.length != 0) mac.update(L2, 0, L2.length);
      mac.doFinal(T2, 0);
    }

    if (T1 == T2) {
      M.slice(0, len) // Decoded plaintext
    } else {
      throw new Exception( s"MACs don't agree. T1: ${T1.hex}, T2: ${T2.hex}" )
    }
  }

  // adapted from org.bouncycastle.crypto.agreement.ECDHBasicAgreement
  def calculateSharedSecret( myPriv : ECPrivateKeyParameters, yourPub : ECPublicKeyParameters ) : Array[Byte] = {
    ECPoint P = yourPub.getQ().multiply(myPriv.getD()).normalize();

    if (P.isInfinity()) throw new IllegalStateException("Infinity is not a valid agreement value for ECDH");

    val fieldSize = (myPriv.getParameters().getCurve().getFieldSize() + 7) / 8;

    return P.getAffineXCoord().toBigInteger().asUnsignedByteArray( fieldSize )
  }

  def readEncodedPublicKey( keyBytes : Array[Byte] ) : ECPublicKeyParemeters = {
    val point = Curve.decodePoint(ephemBytes)
    new ECPublicKeyParameters( point, Curve )
  }

  def encodePublicKey( pub : ECPublicKeyParemeters ) : Array[Byte] = pub.getQ.getEncoded(false)

  private def encryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPrivateKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = calculateSharedSecret( from, to )
    doEncryptBlock( encodedPublicKey, sharedSecret, initializationVector, bytes, offset, len )
  }

  def encryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPrivateKeyParameters,
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
    to : ECPrivateKeyParameters,
    encodedPublicKeyTo : Array[Byte], 
    initializationVector : Array[Byte], 
    bytes : Array[Byte], 
    offset : Int, 
    len : Int 
  ) : Array[Byte] = {
    val sharedSecret = calculateSharedSecret( from, to )
    doDecryptBlock( encodedPublicKey, sharedSecret, initializationVector, bytes, offset, len )
  }

  def decryptBlock(
    from : ECPrivateKeyParameters, 
    to : ECPrivateKeyParameters,
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

