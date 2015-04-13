package com.mchange.sc.v1.consuela;

import com.mchange.sc.v1.log._;
import MLevel._;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

import java.security.{Signature => JcaSignature};

import java.math.BigInteger;

import com.mchange.sc.v1.consuela.hash.Hash;
import com.mchange.sc.v1.consuela.Implicits._;

package object crypto {
  implicit val logger = MLogger( this );

  object secp256k1 {

    val ValueByteLength = 32;

    private val ECParamBundleName   = "secp256k1";
    private val SigAlgoName         = "NONEwithECDSA";                            
    private val KeyAlgoName         = "EC";

    private val ECGenParamSpec      = new ECGenParameterSpec( ECParamBundleName );


    val ECParamSpec = {
      val algorithmParameters = AlgorithmParameters.getInstance( KeyAlgoName );
      algorithmParameters.init( ECGenParamSpec );
      algorithmParameters.getParameterSpec( classOf[ECParameterSpec] )
    }

    def generate_jce_keypair( randomness : SecureRandom )( implicit provider : jce.Provider ) : KeyPair = {
      val generator = KeyPairGenerator.getInstance(SigAlgoName, provider.code);
      generator.initialize( ECGenParamSpec, randomness );
      generator.generateKeyPair();
    }

    def generate_jce_keys( randomness : SecureRandom )( implicit provider : jce.Provider ) : (ECPrivateKey, ECPublicKey) = {
      val jceKeyPair = generate_jce_keypair( randomness )( provider );
      ( jceKeyPair.getPrivate.asInstanceOf[ECPrivateKey], jceKeyPair.getPublic.asInstanceOf[ECPublicKey] )
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

    def generate_bytes_keypair( randomness : SecureRandom )( implicit provider : jce.Provider ) : (Array[Byte], Array[Byte]) = {
      val ( ecpriv, ecpub ) = generate_jce_keys( randomness )( provider );
      ( privkey_bytes( ecpriv ), pubkey_bytes( ecpub ) )
    }

    def jce_private_key_from_S( privateKeyAsS : BigInteger )( implicit provider : jce.Provider ) : ECPrivateKey = {
      val kf = KeyFactory.getInstance( KeyAlgoName ); // XXX: is this KeyFactory immutable or thread-safe? can i cache it?
      val privSpec = new ECPrivateKeySpec( privateKeyAsS, ECParamSpec );
      kf.generatePrivate( privSpec ).asInstanceOf[ECPrivateKey];
    }

    def jce_public_key_from_XY( pubKeyX : BigInteger, pubKeyY : BigInteger )( implicit provider : jce.Provider ) : ECPublicKey = {
      val kf = KeyFactory.getInstance( KeyAlgoName ); // XXX: is this KeyFactory immutable or thread-safe? can i cache it?
      val pubKeyAsPointW = new ECPoint( pubKeyX, pubKeyY );
      val pubSpec = new ECPublicKeySpec( pubKeyAsPointW, ECParamSpec );
      kf.generatePublic( pubSpec ).asInstanceOf[ECPublicKey];
    }

    def signatureBytes( privateKeyAsS : BigInteger, signMe : Array[Byte] )( implicit provider : jce.Provider ) : Array[Byte] = {
      val ecPrivKey = jce_private_key_from_S( privateKeyAsS )( provider ); //algo params are encoded here
      val signer = JcaSignature.getInstance(SigAlgoName, provider.code);
      signer.initSign( ecPrivKey );
      signer.update( signMe );
      signer.sign()
    }

    def parseSignature( signatureBytes : Array[Byte] )( implicit provider : jce.Provider ) : Either[Array[Byte],Signature] = {
      val parser : SignatureParser = findSignatureParser( provider );
      parser.parse( signatureBytes );
    }

    def encodeSignature( signature : Signature )( implicit provider : jce.Provider ) : Array[Byte] = {
      val parser : SignatureParser = findSignatureParser( provider );
      parser.encode( signature );
    }

    def signature( privateKeyAsS : BigInteger, signMe : Array[Byte] )( implicit provider : jce.Provider ) : Either[Array[Byte],Signature] = {
      parseSignature( signatureBytes( privateKeyAsS : BigInteger, signMe : Array[Byte] )( provider ) )( provider )
    }

    private def findSignatureParser( implicit provider : jce.Provider ) : SignatureParser = {
      provider.code match {
        case "BC" => BouncyCastleSignatureParser;
        case _    => BouncyCastleSignatureParser; //all we have for now, but we'll see over time...
      }
    }

    def verifySignatureBytes( signed : Array[Byte], signatureBytes : Array[Byte], pubKeyX : BigInteger, pubKeyY : BigInteger )( implicit provider : jce.Provider ) : Boolean = {
      val ecPubKey = jce_public_key_from_XY( pubKeyX, pubKeyY )( provider ); //algo params are encoded here
      val signer = JcaSignature.getInstance(SigAlgoName, provider.code);
      signer.initVerify( ecPubKey );
      signer.update( signed );
      signer.verify( signatureBytes )
    }

    def verifySignature( signed : Array[Byte], signature : Signature, pubKeyX : BigInteger, pubKeyY : BigInteger )( implicit provider : jce.Provider ) : Boolean = {
      val parser : SignatureParser = findSignatureParser( provider );
      verifySignatureBytes( signed, parser.encode( signature ), pubKeyX, pubKeyY )( provider )
    }

    case class Signature( val r : BigInteger, s : BigInteger, v : Option[Int] = None);
    trait SignatureParser {
      def parse( sigBytes : Array[Byte] ) : Either[Array[Byte],Signature];
      def encode( sig : Signature ) : Array[Byte];
    }

    object BouncyCastleSignatureParser extends SignatureParser {
      import java.io._;
      import org.bouncycastle.asn1._;
      import com.mchange.sc.v2.lang.borrow;

      def parse( sigBytes : Array[Byte] ) : Either[Array[Byte],Signature] = {
        def decodeInteger( i : ASN1Integer ) : BigInteger = i.getValue();

        borrow( new ASN1InputStream( new ByteArrayInputStream( sigBytes ) ) ) { ais =>
          try {
            val primitive : ASN1Primitive = ais.readObject();
            val sequence  : DLSequence = primitive.asInstanceOf[DLSequence];
            Right( Signature( decodeInteger( sequence.getObjectAt(0).asInstanceOf[ASN1Integer] ), decodeInteger( sequence.getObjectAt(1).asInstanceOf[ASN1Integer] ) ) )
          } catch {
            case e : Exception => {
              WARNING.log( s"Unable to parse signature '${sigBytes.hex}'", e );
              Left(sigBytes)
            }
          }
        }
      }
      def encode( sig : Signature ) : Array[Byte] = {
        // grrr...
        class AutocloseableASN1OutputStream( os : OutputStream ) extends ASN1OutputStream( os ) with AutoCloseable;
        borrow( new ByteArrayOutputStream() ) { baos =>
          borrow( new AutocloseableASN1OutputStream( baos ) ) { aos =>
            val encodedR = new ASN1Integer( sig.r );
            val encodedS = new ASN1Integer( sig.s );
            val sequence = new DLSequence( Array[ASN1Encodable]( encodedR, encodedS ) );
            aos.writeObject( sequence );
            aos.close();
            sig.v.foreach( v => WARNING.log( s"Found v value ${v}; It will not be encoded as ${this} does not include v in its binary signature format." ) );
            baos.toByteArray
          }
        }
      }
    }

    /**
     * Derived from https://github.com/ethereum/ethereumj/blob/master/ethereumj-core/src/main/java/org/ethereum/crypto/ECKey.java
     * which is in turn derived from https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/com/google/bitcoin/core/ECKey.java
     */   
    object PublicKeyComputer {
      import org.bouncycastle.asn1.sec.SECNamedCurves;
      import org.bouncycastle.asn1.x9.X9ECParameters;
      import org.bouncycastle.crypto.params.ECDomainParameters;

      val Params = SECNamedCurves.getByName(ECParamBundleName);
      val Curve = new ECDomainParameters(Params.getCurve(), Params.getG(), Params.getN(), Params.getH());

      private val WarningSource = this.getClass.getName;
      jce.Provider.warnForbidUnconfiguredUseOfBouncyCastle( WarningSource )

      def computePublicKeyBytes( privateKeyAsBigInteger : BigInteger ) : Array[Byte] = {
        Curve.getG().multiply( privateKeyAsBigInteger ).getEncoded( false ).drop(1); // false means uncompressed, we drop the header byte that says so
      }

      def computePublicKeyBytes( privateKeyAsBigInt : BigInt ) : Array[Byte] = computePublicKeyBytes( privateKeyAsBigInt.bigInteger );
    }

    /*
     Looks like we don't need this, we've found a standard way to get an ECParameterSpec
     by name (above). But it took me a while to find these constants, so I'm keeping this around.

    private val UnseededECParamSpec = createECParamSpec( null );

    private object CurveConstants {
      private def ubi( str : String ) = str.decodeHex.toUnsignedBigInteger

      // Constants taken from source of Santuario, http://santuario.apache.org

      val ECFieldPrime = ubi("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f")
      val A            = ubi("0000000000000000000000000000000000000000000000000000000000000000");
      val B            = ubi("0000000000000000000000000000000000000000000000000000000000000007");
      val X            = ubi("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798");
      val Y            = ubi("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8");
      val N            = ubi("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141");
      val H            = 1;
    }
    private def createECParamSpec( seed : Array[Byte] ) = {
      import java.security.spec.ECFieldFp;
      import java.security.spec.EllipticCurve;
      import java.security.spec.ECPoint;
      import CurveConstants._;

      // This code is never tried, very much unverified, the result
      // of some guesswork and reverse-engineering on my part.
      // Resuscitate only with great care.

      val field = new ECFieldFp( ECFieldPrime );
      val curve = new EllipticCurve( field, A, B, seed );
      val g     = new ECPoint( X, Y );

      ECParameterSpec( curve, g, N, H );
    }
    */
  }
}
