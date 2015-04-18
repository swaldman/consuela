package com.mchange.sc.v1.consuela;

import com.mchange.sc.v1.log._;
import MLevel._;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

import java.security.{Signature => JcaSignature};

import java.math.BigInteger;
import java.util.Arrays;
import scala.util.hashing.MurmurHash3;

import com.mchange.sc.v1.consuela.hash.Hash;
import com.mchange.sc.v1.consuela.Implicits._;

package object crypto {
  class InvalidSignatureException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class ForbiddenProviderException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );

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
      val generator = KeyPairGenerator.getInstance(SigAlgoName, provider.name);
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
      val signer = JcaSignature.getInstance(SigAlgoName, provider.name);
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
      val out = {
        provider match {
          case jce.Provider.BouncyCastle => BouncyCastleSignatureParser;
          case jce.Provider.SpongyCastle => SpongyCastleSignatureParser;
          case _                         => BouncyCastleSignatureParser;
        }
      }
      jce.Provider.warnForbidUnavailableProvider( "findSignatureParser(...)", out.implementingProvider )( provider )
      out
    }

    private def findPublicKeyComputer( implicit provider : jce.Provider ) : PublicKeyComputer = {
      val out = {
        provider match {
          case jce.Provider.BouncyCastle => BouncyCastlePublicKeyComputer;
          case jce.Provider.SpongyCastle => SpongyCastlePublicKeyComputer;
          case _                         => BouncyCastlePublicKeyComputer;
        }
      }
      jce.Provider.warnForbidUnavailableProvider( "findPublicKeyComputer(...)", out.implementingProvider )( provider )
      out
    }

    def verifySignatureBytes( signed : Array[Byte], signatureBytes : Array[Byte], pubKeyX : BigInteger, pubKeyY : BigInteger )( implicit provider : jce.Provider ) : Boolean = {
      val ecPubKey = jce_public_key_from_XY( pubKeyX, pubKeyY )( provider ); //algo params are encoded here
      val signer = JcaSignature.getInstance(SigAlgoName, provider.name);
      signer.initVerify( ecPubKey );
      signer.update( signed );
      signer.verify( signatureBytes )
    }

    def verifySignature( signed : Array[Byte], signature : Signature, pubKeyX : BigInteger, pubKeyY : BigInteger )( implicit provider : jce.Provider ) : Boolean = {
      val parser : SignatureParser = findSignatureParser( provider );
      verifySignatureBytes( signed, parser.encode( signature ), pubKeyX, pubKeyY )( provider )
    }

    case class Signature( val r : BigInteger, s : BigInteger, v : Option[Byte] = None);
    trait SignatureParser {
      def implementingProvider : jce.Provider;
      def parse( sigBytes : Array[Byte] ) : Either[Array[Byte],Signature];
      def encode( sig : Signature ) : Array[Byte];
    }

    /*
     * Note that "v" here refers to a standardized attribute of our signature (not "value" as
     * the pairing with "key" might suggest). See e.g. the Ethereum yellow paper for information.
     *
     * recId should fall in the range [0,3]; v within [27,30]; they represent the same value, just offset by 27
     *
     * we don't test the constructor arguments, since they will have all been tested as pre- 
     * or post-conditions of recoverPublicKeyBytes(...)
     */ 
    final class RecoveredPublicKeyAndV private[crypto]( val recId : Int, val publicKeyBytes : Array[Byte] ) {
      override def equals( other : Any ) : Boolean = {
        other match {
          case rkv : RecoveredPublicKeyAndV => this.recId == rkv.recId && Arrays.equals( this.publicKeyBytes, rkv.publicKeyBytes );
          case _                            => false;
        }
      }
      override def hashCode : Int = this.recId ^ MurmurHash3.bytesHash( this.publicKeyBytes );

      def v = vFromRecId( recId );
    }

    def vFromRecId( recId : Int ) = recId + 27;
    def recIdFromV( v : Int )     = v     - 27;

    /**
      *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
      *          of two 256 bit big-endian ints X and Y
      */ 
    def computePublicKeyBytes( privateKeyAsBigInteger : BigInteger )( implicit provider : jce.Provider ) : Array[Byte] = {
      findPublicKeyComputer( provider ).computePublicKeyBytes( privateKeyAsBigInteger )( provider )
    }

    /**
      *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
      *          of two 256 bit big-endian ints X and Y
      */ 
    def recoverPublicKeyBytesV( v : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
      findPublicKeyComputer( provider ).recoverPublicKeyBytesV( v, sigR, sigS, signed )( provider )
    }

    /**
      *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
      *          of two 256 bit big-endian ints X and Y
      */ 
    def recoverPublicKeyBytesRecId( recId : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
      findPublicKeyComputer( provider ).recoverPublicKeyBytesRecId( recId, sigR, sigS, signed )( provider )
    }

    def recoverPublicKeyAndV( sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[RecoveredPublicKeyAndV] = {
      findPublicKeyComputer( provider ).recoverPublicKeyAndV( sigR, sigS, signed )( provider )
    }

    private trait PublicKeyComputer {
      def implementingProvider : jce.Provider;

      /**
        *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
        *          of two 256 bit big-endian ints X and Y
        */ 
      def computePublicKeyBytes( privateKeyAsBigInteger : BigInteger )( implicit provider : jce.Provider ) : Array[Byte];

      /**
        *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
        *          of two 256 bit big-endian ints X and Y
        */ 
      def recoverPublicKeyBytesV( v : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]];

      /**
        *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
        *          of two 256 bit big-endian ints X and Y
        */ 
      def recoverPublicKeyBytesRecId( recId : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]];

      def recoverPublicKeyAndV( sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[RecoveredPublicKeyAndV];
    }

    /*
     * 
     * (Unfortunately) Provider-specific APIs
     * 
     */ 

    // BouncyCastle

    // ICKY, BUT REMEMBER: EDITS TO BouncyCastle STUFF IMPLY PARALLEL EDITS TO SpongyCastle STUFF.
    // THIS SHOULD BE REPLACED BY CODEGENERATION OR MACROS OR SOMETHING SOMEDAY.

    /**
      * Derived from https://github.com/ethereum/ethereumj/blob/master/ethereumj-core/src/main/java/org/ethereum/crypto/ECKey.java
      * which is in turn derived from https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/com/google/bitcoin/core/ECKey.java
      */   
    private object BouncyCastlePublicKeyComputer extends PublicKeyComputer {
      import org.bouncycastle.asn1.sec.SECNamedCurves;
      import org.bouncycastle.asn1.x9.X9IntegerConverter;
      import org.bouncycastle.asn1.x9.X9ECParameters;
      import org.bouncycastle.crypto.params.ECDomainParameters;
      import org.bouncycastle.math.ec.ECAlgorithms;
      import org.bouncycastle.math.ec.ECCurve;
      import org.bouncycastle.math.ec.ECPoint;

      override val implementingProvider : jce.Provider = jce.Provider.BouncyCastle;

      val Params = SECNamedCurves.getByName(ECParamBundleName);
      val Curve = new ECDomainParameters(Params.getCurve(), Params.getG(), Params.getN(), Params.getH());

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def computePublicKeyBytes( privateKeyAsBigInteger : BigInteger )( implicit provider : jce.Provider ) : Array[Byte] = {
        val rawKey = Curve.getG().multiply( privateKeyAsBigInteger ).getEncoded( false );
        assert( rawKey(0) == 0x04 && rawKey.length == 65, "Computed public key is not in the expected uncompressed format." );
        rawKey.drop(1)  // drop the header byte 0x04 that signifies an uncompressed concatenation of values
      }

      def recoverPublicKeyAndV( sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[RecoveredPublicKeyAndV] = {
        (0 to 3).foldLeft( None : Option[RecoveredPublicKeyAndV] ){ ( mbr, i ) =>
          if (mbr == None) {
            recoverPublicKeyBytesRecId( i, sigR, sigS, signed )( provider ).fold( None : Option[RecoveredPublicKeyAndV] )( bytes => Some( new RecoveredPublicKeyAndV( i, bytes ) ) )
          } else { 
            mbr
          }
        }
      }

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def recoverPublicKeyBytesV( v : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
        recoverPublicKeyBytesRecId( recIdFromV(v), sigR, sigS, signed )( provider );
      }

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def recoverPublicKeyBytesRecId( recId : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
        require(recId >= 0);
        require(sigR.signum() >= 0);
        require(sigS.signum() >= 0);
        require(signed != null);
        val n     = Curve.getN();  // Curve order.
        val i     = BigInteger.valueOf(recId / 2);
        val x     = sigR.add(i.multiply(n));
        val curve = Curve.getCurve().asInstanceOf[ECCurve.Fp];

        def decompressKey( xBN : BigInteger, yBit : Boolean) : ECPoint = {
          val x9 = new X9IntegerConverter();
          val compEnc = x9.integerToBytes(xBN, 1 + x9.getByteLength(curve));
          compEnc(0) = (if (yBit) 0x03 else 0x02).toByte;
          curve.decodePoint(compEnc);
        }

        val prime = curve.getQ();  // Bouncy Castle is not consistent about the letter it uses for the prime.
        if (x.compareTo(prime) >= 0) {
          TRACE.log(
            s"recoverPublicKeyBytes(...) returning NONE for recId ${recId}: Public key cannot have implied x value ${x}, as that is greater than its prime modulus ${prime}"
          );
          None
        } else {
          val R = decompressKey(x, (recId & 1) == 1);
          if (!R.multiply(n).isInfinity()) {
            TRACE.log( s"recoverPublicKeyBytes(...) returning NONE: Unexpectedly non-infinte value of nR; try another recId [current recId: ${recId}]." )
            None
          } else {
            val e = new BigInteger(1, signed);
            val eInv = BigInteger.ZERO.subtract(e).mod(n);
            val rInv = sigR.modInverse(n);
            val srInv = rInv.multiply(sigS).mod(n);
            val eInvrInv = rInv.multiply(eInv).mod(n);
            val q = ECAlgorithms.sumOfTwoMultiplies(Curve.getG(), eInvrInv, R, srInv).asInstanceOf[ECPoint.Fp];

            // we have to drop a header byte indicating the lack of encodedness
            Some( q.getEncoded(false).drop(1).ensuring( _.length == 64 ) )
          }
        }
      }
    }

    private object BouncyCastleSignatureParser extends SignatureParser {
      import java.io._;
      import org.bouncycastle.asn1._;
      import com.mchange.sc.v2.lang.borrow;

      override val implementingProvider : jce.Provider = jce.Provider.BouncyCastle;

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

    // SpongyCastle for Android

    /**
      * Derived from https://github.com/ethereum/ethereumj/blob/master/ethereumj-core/src/main/java/org/ethereum/crypto/ECKey.java
      * which is in turn derived from https://github.com/bitcoinj/bitcoinj/blob/master/core/src/main/java/com/google/bitcoin/core/ECKey.java
      */   
    private object SpongyCastlePublicKeyComputer extends PublicKeyComputer {
      import org.spongycastle.asn1.sec.SECNamedCurves;
      import org.spongycastle.asn1.x9.X9IntegerConverter;
      import org.spongycastle.asn1.x9.X9ECParameters;
      import org.spongycastle.crypto.params.ECDomainParameters;
      import org.spongycastle.math.ec.ECAlgorithms;
      import org.spongycastle.math.ec.ECCurve;
      import org.spongycastle.math.ec.ECPoint;

      override val implementingProvider : jce.Provider = jce.Provider.SpongyCastle;

      val Params = SECNamedCurves.getByName(ECParamBundleName);
      val Curve = new ECDomainParameters(Params.getCurve(), Params.getG(), Params.getN(), Params.getH());

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def computePublicKeyBytes( privateKeyAsBigInteger : BigInteger )( implicit provider : jce.Provider ) : Array[Byte] = {
        val rawKey = Curve.getG().multiply( privateKeyAsBigInteger ).getEncoded( false );
        assert( rawKey(0) == 0x04 && rawKey.length == 65, "Computed public key is not in the expected uncompressed format." );
        rawKey.drop(1)  // drop the header byte 0x04 that signifies an uncompressed concatenation of values
      }

      def recoverPublicKeyAndV( sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[RecoveredPublicKeyAndV] = {
        (0 to 3).foldLeft( None : Option[RecoveredPublicKeyAndV] ){ ( mbr, i ) =>
          if (mbr == None) {
            recoverPublicKeyBytesRecId( i, sigR, sigS, signed )( provider ).fold( None : Option[RecoveredPublicKeyAndV] )( bytes => Some( new RecoveredPublicKeyAndV( i, bytes ) ) )
          } else { 
            mbr
          }
        }
      }

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def recoverPublicKeyBytesV( v : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
        recoverPublicKeyBytesRecId( recIdFromV(v), sigR, sigS, signed )( provider );
      }

      /**
       *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
       *          of two 256 bit big-endian ints X and Y
       */ 
      def recoverPublicKeyBytesRecId( recId : Int, sigR : BigInteger, sigS : BigInteger, signed : Array[Byte] )( implicit provider : jce.Provider ) : Option[Array[Byte]] = {
        require(recId >= 0);
        require(sigR.signum() >= 0);
        require(sigS.signum() >= 0);
        require(signed != null);
        val n     = Curve.getN();  // Curve order.
        val i     = BigInteger.valueOf(recId / 2);
        val x     = sigR.add(i.multiply(n));
        val curve = Curve.getCurve().asInstanceOf[ECCurve.Fp];

        def decompressKey( xBN : BigInteger, yBit : Boolean) : ECPoint = {
          val x9 = new X9IntegerConverter();
          val compEnc = x9.integerToBytes(xBN, 1 + x9.getByteLength(curve));
          compEnc(0) = (if (yBit) 0x03 else 0x02).toByte;
          curve.decodePoint(compEnc);
        }

        val prime = curve.getQ();  // Bouncy Castle is not consistent about the letter it uses for the prime.
        if (x.compareTo(prime) >= 0) {
          TRACE.log(
            s"recoverPublicKeyBytes(...) returning NONE for recId ${recId}: Public key cannot have implied x value ${x}, as that is greater than its prime modulus ${prime}"
          );
          None
        } else {
          val R = decompressKey(x, (recId & 1) == 1);
          if (!R.multiply(n).isInfinity()) {
            TRACE.log( s"recoverPublicKeyBytes(...) returning NONE: Unexpectedly non-infinte value of nR; try another recId [current recId: ${recId}]." )
            None
          } else {
            val e = new BigInteger(1, signed);
            val eInv = BigInteger.ZERO.subtract(e).mod(n);
            val rInv = sigR.modInverse(n);
            val srInv = rInv.multiply(sigS).mod(n);
            val eInvrInv = rInv.multiply(eInv).mod(n);
            val q = ECAlgorithms.sumOfTwoMultiplies(Curve.getG(), eInvrInv, R, srInv).asInstanceOf[ECPoint.Fp];

            // we have to drop a header byte indicating the lack of encodedness
            Some( q.getEncoded(false).drop(1).ensuring( _.length == 64 ) )
          }
        }
      }
    }

    private object SpongyCastleSignatureParser extends SignatureParser {
      import java.io._;
      import org.spongycastle.asn1._;
      import com.mchange.sc.v2.lang.borrow;

      override val implementingProvider : jce.Provider = jce.Provider.SpongyCastle;

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

    /*
     Public key encoding formats (from http://sourceforge.net/p/bitcoin/mailman/message/29416133/):
     
     In total, the following encodings exist:
     * 0x00: point at infinity; not a valid public key
     * 0x02 [32-byte X coord]: compressed format for even Y coords
     * 0x03 [32-byte X coord]: compressed format for odd Y coords
     * 0x04 [32-byte X coord] [32-byte Y coord]: uncompressed format
     * 0x06 [32-byte X coord] [32-byte Y coord]: hybrid format for even Y coords
     * 0x07 [32-byte X coord] [32-byte Y coord]: hybrid format for odd Y coords

     */

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
