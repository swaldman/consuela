package com.mchange.sc.v1.consuela;

import com.mchange.sc.v1.log._;
import MLevel._;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

import com.mchange.sc.v1.consuela.Implicits._;

package object crypto {
  implicit val logger = MLogger( this );

  object secp256k1 {

    val ValueByteLength = 32;

    private val Spec = new ECGenParameterSpec("secp256k1");

    def generate_jce_keypair( randomness : SecureRandom )( implicit provider : jce.Provider ) : KeyPair = {
      val generator = KeyPairGenerator.getInstance("ECDSA", provider.code);
      generator.initialize( Spec, randomness );
      generator.generateKeyPair();
    }

    def generate_jce_keys( randomness : SecureRandom )( implicit provider : jce.Provider ) : (ECPublicKey, ECPrivateKey) = {
      val jceKeyPair = generate_jce_keypair( randomness )( provider );
      ( jceKeyPair.getPublic.asInstanceOf[ECPublicKey], jceKeyPair.getPrivate.asInstanceOf[ECPrivateKey] )
    }

    def pubkey_bigints( ecPub : ECPublicKey ) : ( BigInt, BigInt ) = {
      val ecPoint = ecPub.getW();
      ( ecPoint.getAffineX(), ecPoint.getAffineY() )
    }

    def privkey_bigint( ecPriv : ECPrivateKey ) : BigInt = ecPriv.getS()

    def pubkey_bytes( ecPub : ECPublicKey ) : Array[Byte] = {
      val ecPoint = ecPub.getW();
      ecPoint.getAffineX().unsignedBytes(ValueByteLength) ++ ecPoint.getAffineY().unsignedBytes(ValueByteLength)
    }

    def privkey_bytes( ecPriv : ECPrivateKey ) : Array[Byte] = ecPriv.getS().unsignedBytes(ValueByteLength)

    /**
     * Derived from https://github.com/ethereum/ethereumj/blob/master/ethereumj-core/src/main/java/org/ethereum/crypto/ECKey.java
     * which is in turn derived from https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/com/google/bitcoin/core/ECKey.java
     */   
    object PublicKeyComputer {
      import org.bouncycastle.asn1.sec.SECNamedCurves;
      import org.bouncycastle.asn1.x9.X9ECParameters;
      import org.bouncycastle.crypto.params.ECDomainParameters;

      val Params = SECNamedCurves.getByName("secp256k1");
      val Curve = new ECDomainParameters(Params.getCurve(), Params.getG(), Params.getN(), Params.getH());

      private val WarningSource = "com.mchange.sc.v1.consuela.secp256k1.PublicKeyComputer";
      jce.Provider.warnForbidUnconfiguredUseOfBouncyCastle( WarningSource )

      def computePublicKeyBytes( privateKeyAsBigInteger : java.math.BigInteger ) : Array[Byte] = {
        Curve.getG().multiply( privateKeyAsBigInteger ).getEncoded( false ).drop(1); // false means uncompressed, we drop the header byte that says so
      }

      def computePublicKeyBytes( privateKeyAsBigInt : BigInt ) : Array[Byte] = computePublicKeyBytes( privateKeyAsBigInt.bigInteger );
    }
  }
}
