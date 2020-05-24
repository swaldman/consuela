package com.mchange.sc.v1.consuela.ethereum.net.rlpx;

import java.security.SecureRandom;
import javax.crypto.Cipher;

import com.mchange.sc.v1.consuela.{crypto,MainProvider};
import com.mchange.sc.v1.consuela.ethereum.{EthPrivateKey,EthPublicKey};

object ECIES {
  val Default : ECIES = new BouncyCastleCipherECIES( () => new SecureRandom )

  val EmptyByteArray = Array.ofDim[Byte](0)

  val MacKeyLength               = 128
  val InitializationVectorLength = 16

  final object BouncyCastleCipherECIES {
    import org.bouncycastle.crypto.agreement.ECDHBasicAgreement;
    import org.bouncycastle.crypto.digests.SHA256Digest;
    import org.bouncycastle.crypto.engines.AESEngine;
    import org.bouncycastle.crypto.engines.IESEngine;
    import org.bouncycastle.jcajce.provider.asymmetric.ec.IESCipher;
    import org.bouncycastle.crypto.macs.HMac;
    import org.bouncycastle.crypto.generators.KDF2BytesGenerator;
    import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
    import org.bouncycastle.crypto.modes.SICBlockCipher;

    object CustomCipher {
      def createEngine() : IESEngine = {
        new IESEngine(
          new ECDHBasicAgreement(), 
          new KDF2BytesGenerator(new SHA256Digest()), 
          new HMac(new SHA256Digest()), 
          new PaddedBufferedBlockCipher(new SICBlockCipher(new AESEngine()))
        )
      }
    }
    class CustomCipher extends IESCipher( CustomCipher.createEngine(), InitializationVectorLength );
  }
  class BouncyCastleCipherECIES( randomFactory : () => SecureRandom ) extends ECIES( randomFactory ){
    import org.bouncycastle.jce.spec.IESParameterSpec;
    import org.bouncycastle.crypto.params.ParametersWithIV;

    def encrypt( pubkey : EthPublicKey, plaintext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {

      val ecPublicKey = crypto.secp256k1.jce_public_key_from_XY( pubkey.x.bigInteger, pubkey.y.bigInteger )( provider )
      val cipher = new BouncyCastleCipherECIES.CustomCipher
      val random = randomFactory()

      val IV = Array.ofDim[Byte]( InitializationVectorLength )
      random.nextBytes( IV )
      val p = new IESParameterSpec( ECIES.EmptyByteArray, ECIES.EmptyByteArray, 128, 128, IV )
      cipher.engineInit( Cipher.ENCRYPT_MODE, ecPublicKey, p, random )

      cipher.engineDoFinal( plaintext, 0, plaintext.length )
    }
    def decrypt( pvtkey : EthPrivateKey, ciphertext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = ???
  }

  /*
  final object BouncyCastleECIES {
    import org.bouncycastle.crypto.AsymmetricCipherKeyPair;
    import org.bouncycastle.crypto.agreement.ECDHBasicAgreement;
    import org.bouncycastle.crypto.digests.SHA256Digest;
    import org.bouncycastle.asn1.sec.SECNamedCurves;
    import org.bouncycastle.crypto.generators.KDF2BytesGenerator;
    import org.bouncycastle.crypto.params.ECDomainParameters;
    import org.bouncycastle.crypto.params.ECPrivateKeyParameters;
    import org.bouncycastle.crypto.params.ECPublicKeyParameters;
    import org.bouncycastle.math.ec.ECCurve;

    val ECParamBundleName = "secp256k1"
    val NamedCurve        = SECNamedCurves.getByName(ECParamBundleName);
    val CurveParams       = new ECDomainParameters(NamedCurve.getCurve(), NamedCurve.getG(), NamedCurve.getN(), NamedCurve.getH());
    val Curve             = CurveParams.getCurve().asInstanceOf[ECCurve.Fp]

    val IESParams = new IESParameters( EmptyByteArray, EmptyByteArray, MacKeyLength )

    def cipherParams( random : java.security.SecureRandom ) = {
      val arr = Array.ofDim[Byte]( InitializationVectorLength )
      random.nextBytes( arr )
      new ParametersWithIV( IESParams, arr )
    }

    def ecPrivateKeyParams( privateKey : EthPrivateKey ) : ECPrivateKeyParameters = new ECPrivateKeyParameters( privateKey.s.bigInteger, CurveParams )
    def ecPublicKeyParams( publicKey : EthPublicKey )    : ECPublicKeyParameters  = new ECPublicKeyParameters( Curve.decodePoint(publicKey.toByteArray), CurveParams )

    def ecKeyPair( publicKey : EthPublicKey, privateKey : EthPrivateKey ) : AsymmetricCipherKeyPair = {
      val ecPub = ecPublicKeyParams( publicKey )
      val ecPvt = ecPrivateKeyParams( privateKey )
      new AsymmetricCipherKeyPair( ecPub, ecPvt )
    }

    def createEngine = new IESEngine( new ECDHBasicAgreement, new KDF2BytesGenerator(new SHA3Digest(256)), new HMac(new SHA3Digest(256)) );
  }
  class BouncyCastleECIES( randomFactory : () => SecureRandom ) extends ECIES( randomFactory ){
  }

  class DefaultJceECIES( randomFactory : () => SecureRandom ) extends ECIES( randomFactory ){
    val CipherName = "ECIESwithAES"

    def encrypt( pubkey : EthPublicKey, plaintext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {

      val ecPublicKey = crypto.secp256k1.jce_public_key_from_XY( pubkey.x.bigInteger, pubkey.y.bigInteger )( provider )
      val cipher = Cipher.getInstance( CipherName, provider.name )
      val random = randomFactory()

      // YUK... PARAMS DON'T GET THROUGH FOR SOME REASON
      //
      // hopefully unnecessary... UNHARDCODE IF IT BECOMES NECESSARY
      //val IV = Array.ofDim[Byte]( 16 )
      //random.nextBytes( IV )
      //val p = new org.bouncycastle.jce.spec.IESParameterSpec( ECIES.EmptyByteArray, ECIES.EmptyByteArray, 128, 128, IV )
      //cipher.init( Cipher.ENCRYPT_MODE, ecPublicKey, p, random )

      cipher.init( Cipher.ENCRYPT_MODE, ecPublicKey, random )

      cipher.doFinal( plaintext, 0, plaintext.length )
    }
    def decrypt( pvtkey : EthPrivateKey, ciphertext : Array[Byte] ) : Array[Byte] = ???
  }
  */ 
}
abstract class ECIES( val randomFactory : () => SecureRandom ) {
  def encrypt( pubkey : EthPublicKey, plaintext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte];
  def decrypt( pvtkey : EthPrivateKey, ciphertext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte];
}

