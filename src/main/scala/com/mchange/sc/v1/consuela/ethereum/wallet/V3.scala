package com.mchange.sc.v1.consuela.ethereum.wallet

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.{clients,EthAddress,EthHash,EthPrivateKey}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact20,ByteSeqExact32}

import com.mchange.sc.v2.lang.borrow

import play.api.libs.json._

import java.io.{InputStream,BufferedInputStream,File,FileInputStream,OutputStream}
import java.lang.{Exception => JException}
import java.security.SecureRandom
import java.util.UUID
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}

import scala.io.Codec

/*
     Some stuff:

     I haven't seen any kind of spec for the V3 wallet, so here are a few scrounged-from-github or
     reverse engineered notes. 

     See especially https://github.com/ethereumjs/ethereumjs-wallet#remarks-about-tov3

     First, a sample wallet...

       {
         "address" : "4aa20155e73604737c6701235cd8f6a3d36287d7",
         "crypto" : {
           "cipher" :"aes-128-ctr",
           "ciphertext" : "412839ad15ed865a3d206cdc0a804f575b70c56106e859c393a2c80aa7d4d114",
           "cipherparams" : {
             "iv" : "6b66e9c45cbd4ce7b44cc56899a7863c"
           },
           "kdf" : "scrypt",
           "kdfparams" : {
             "dklen" : 32,
             "n" : 262144,
             "p" : 1,
             "r" : 8,
             "salt" : "0984e3493604f9749c78ff86f6c436c52cab0807157394d0a0b6b2bcf30a01ef"
           },
           "mac" : "3d5af77596f683426332f0270f7b80a323aae79f4aaa55d4ff59c3452694dafd"
         },
         "id" : "3abe4274-d44b-4231-b176-24a97830dc82",
         "version" : 3
       }

     The only supported cipher is "aes-128-ctr".

     A V3 wallet is basically an EthPrivateKey encrypted using 128 AES CTR ("AES/CTR/NoPadding" in JCE-speak).

     The key is always, therefore 16 bytes. 

     However the key is not used directly, but derived from a passphrase
     via a key derivation function, with "scrypt" or "pbkdf2"

     A 16-byte initialization vector is also used to initialize the main AES cipher.

     The two supported KDF functions (scrypt and pbkdf2) share the following parameters in common:

         "salt"  - 32 bytes of hex-encoded randomness
         "dklen" - a natural number, the length of the key that should be generated (default is 32)

     dklen is VERY CONFUSING. After all, we know that we are trying to generate 16 byte keys.
     You MUST choose a dklen of at least 32, however. 

      - Bytes [0,16)  become the key.

      - Bytes [16,32) are prepended to the ciphertext, the result of 
        which is hashed using MAC Ethereum's usuall Keccak256 hash  
        (aliased to EthHash in this library) to yield the MAC.

     As far as I can tell, any key bytes beyond the initial 32 are simply ignored.

     In addition to "salt" and "dklen", the two KDFs also accept the KDF-specific params. 

     I'm taking these directly from https://github.com/ethereumjs/ethereumjs-wallet#remarks-about-tov3

      - "scrypt"
          "n" - Iteration count. Defaults to 262144.
          "r" - Block size for the underlying hash. Defaults to 8.
          "p" - Parallelization factor. Defaults to 1.

      - "pbkdf2"
          "c"   - Number of iterations. Defaults to 262144.
          "prf" - The only supported (and default) value is "hmac-sha256".

     Now that we have the basics, let's go throigh the wallet format. It is a JSON object
     with:

       "address" - A hex-encoded 20-byte String, public EthAddress associated with this wallet

       "crypto"  - A JSON object containing all the cryptographic stuff, including fields:
            "cipher"       - Must be "aes-128-ctr"
            "ciphertext"   - A hex-encoded String, the aes-128-ctr-encoded EthPrivateKey
            "cipherparams" - A JSON object containing precidely one field in the current version
                "iv" - A hex-encoded String, the 16-byte initialization vector
            "kdf"          - one of "scrypt" or "pbkdf2"
            "kdfparams"    - A JSON object containing "dklen", "salt", and kdf-specific params, as described above
            "mac"          - A hex-encoded String, 32 bytes. The MAC can be verified by computing the key with 
                             the KDF, prepending bytes [16,32) of the derived key to the ciphertext, and then 
                             hashing with EthHash (Keccak256)
            
       "id"      - A randomly-generated RFC 4122 Variant 4 UUID identifier of the wallet.
                   See e.g. https://docs.oracle.com/javase/7/docs/api/java/util/UUID.html#randomUUID()

       "version" - The number 3 is the only supported value

 */ 

object V3 {
  class Exception( msg : String ) extends JException( msg )

  // not currently used, because BouncyCastle (non-FIPS) doesn't support.
  // "custom" implemented with BouncyCastle primitives
  private val Pbkdf2AlgoName = "PBKDF2WithHmacSHA256" 

  private val MainCryptAlgoWalletName = "aes-128-ctr"
  private val MainCryptAlgoName       = "AES"
  private val MainCryptAlgoNameFull   = "AES/CTR/NoPadding"

  private val Version = 3

  private val BufferSize = 2048

  /**
    * Does not validate the wallet, but will fail with an Exception if not a JSON object
    */ 
  def apply( is : InputStream ) : V3 = V3( Json.parse(is).asInstanceOf[JsObject] )

  def apply( file : File ) : V3 = borrow ( new BufferedInputStream( new FileInputStream( file ) ) )( apply )

  def generateScrypt(
    passphrase : String,
    n          : Int = 262144,
    r          : Int = 8,
    p          : Int = 1,
    dklen      : Int = 32,
    privateKey : Option[EthPrivateKey] = None,
    random     : SecureRandom = new SecureRandom
  )( implicit provider : jce.Provider ) : V3 = {
    val jso = generateScryptWalletJson( passphrase, n, r, p, dklen, privateKey, random )( provider )
    V3( jso )
  }

  def generatePbkdf2(
    passphrase : String,
    c          : Int = 262144,
    dklen      : Int = 32,
    privateKey : Option[EthPrivateKey] = None,
    random     : SecureRandom = new SecureRandom
  )( implicit provider : jce.Provider ) : V3 = {
    val jso = generatePbkdf2WalletJson( passphrase, c, dklen, privateKey, random )( provider )
    V3( jso )
  }

  private def generateScryptWalletJson(
    passphrase : String,
    n          : Int,
    r          : Int,
    p          : Int,
    dklen      : Int,
    privateKey : Option[EthPrivateKey],
    random     : SecureRandom
  )( implicit provider : jce.Provider ) : JsObject = {
    fillInWalletJson( passphrase, scryptKdfAndParams( n, r, p, dklen, random ), privateKey, random )( provider )
  }

  private def generatePbkdf2WalletJson(
    passphrase : String,
    c          : Int,
    dklen      : Int,
    privateKey : Option[EthPrivateKey],
    random     : SecureRandom
  )( implicit provider : jce.Provider ) : JsObject = {
    fillInWalletJson( passphrase, pbkdf2KdfAndParams( c, dklen, random ), privateKey, random )( provider )
  }

  // yields { "salt" : ???, "dklen" : ??? }
  private def saltDklenKdfParams( dklen : Int, random : SecureRandom ) : JsObject = {
    val saltBytes = {
      val tmp = Array.ofDim[Byte]( 32 )
      random.nextBytes( tmp )
      tmp
    }

    JsObject( Seq( "salt" -> JsString( saltBytes.hex ), "dklen" -> JsNumber( dklen ) ) )
  }

  // yields { "crypto" : { "kdf" : "scrypt", "kdfparams" : { "n" : ???, "r" : ???, "p" : ???, "salt" : ???, "dklen" : ??? } } }
  private def scryptKdfAndParams( n : Int, r : Int, p : Int, dklen : Int, random : SecureRandom ) = {
    val kdfParams = {
      saltDklenKdfParams( dklen, random ) +
        ("n" -> JsNumber(n)) +
        ("r" -> JsNumber(r)) +
        ("p" -> JsNumber(p))
    }
    JsObject( Seq( "crypto" -> JsObject( Seq( "kdf" -> JsString("scrypt"), "kdfparams" -> kdfParams ) ) ) )
  }


  // yields { "crypto" : { "kdf" : "pbkdf2", "kdfparams" : { "c" : ???, "prf" : "hmac-sha256", "salt" : ???, "dklen" : ??? } } }
  private def pbkdf2KdfAndParams( c : Int, dklen : Int, random : SecureRandom ) = {
    val kdfParams : JsObject = {
      saltDklenKdfParams( dklen, random ) +
        ("c" -> JsNumber(c)) +
        ("prf" -> JsString("hmac-sha256"))
    }
    JsObject( Seq( "crypto" -> JsObject( Seq( "kdf" -> JsString("pbkdf2"), "kdfparams" -> kdfParams ) ) ) )
  }

  private def fillInWalletJson( passphrase : String, kdfAndKdfParamsOnly : JsObject, privateKey : Option[EthPrivateKey], random : SecureRandom )( implicit provider : jce.Provider ) : JsObject = {

    val pkey = privateKey.getOrElse( EthPrivateKey( random ) )

    val address = pkey.toPublicKey.toAddress

    val iv = {
      val tmp = Array.ofDim[Byte]( 16 )
      random.nextBytes( tmp )
      tmp
    }

    val keyMac = findKeyMac( kdfAndKdfParamsOnly, passphrase )( provider )

    val ciphertextBytes = encodePrivateKey( pkey, iv, keyMac )( provider )

    val macBytes = keyMac.mac( ciphertextBytes )

    val uuid = UUID.randomUUID()

    val crypto = {
      (kdfAndKdfParamsOnly \ "crypto").as[JsObject] +
        ( "cipher", JsString( MainCryptAlgoWalletName ) ) +
        ( "ciphertext", JsString( ciphertextBytes.hex ) ) +
        ( "cipherparams", JsObject( Seq( "iv" -> JsString( iv.hex ) ) ) ) +
        ( "mac", JsString( macBytes.hex ) )
    }

    JsObject(
      Seq(
        ( "address", JsString( address.bytes.widen.hex ) ),
        ( "crypto", crypto ),
        ( "id", JsString( uuid.toString ) ),
        ( "version", JsNumber( Version ) )
      )
    )
  }

  private def encodePrivateKey( privateKey : EthPrivateKey, iv : Array[Byte], keyMac : KeyMac )( implicit provider : jce.Provider ) : Array[Byte] = {
    val plaintext = privateKey.bytes.widen.toArray

    val cipher = Cipher.getInstance( MainCryptAlgoNameFull, provider.name )
    cipher.init( Cipher.ENCRYPT_MODE, keyMac.key, new IvParameterSpec( iv ) )
    cipher.doFinal( plaintext )
  }

  def decodePrivateKey( walletV3 : V3, passphrase : String )( implicit provider : jce.Provider ) : EthPrivateKey = {
    decodePrivateKey( walletV3.jso, passphrase )( provider )
  }

  private def decodePrivateKey( jsv : JsValue, passphrase : String )( implicit provider : jce.Provider ) : EthPrivateKey = {

    val _version = version( jsv )

    if (_version != 3)
      throw new V3.Exception( s"Excepted a V3 wallet but found version ${_version}" )

    val _cipher          = cipher( jsv )( provider )
    val _keyMac          = findKeyMac( jsv, passphrase )( provider )
    val _ivParameterSpec = new IvParameterSpec( iv(jsv) )
    val _ciphertext      = ciphertext( jsv )

    val _mac = mac( jsv )

    if ( _keyMac.mac( _ciphertext ) != _mac )
      throw new V3.Exception( s"Message authetication code mismatch: KDF-derived MAC: ${_keyMac.mac( _ciphertext ).hex}, expected MAC: ${_mac.hex}" )

    val out = decodePrivateKey( _cipher, _keyMac.key, _ivParameterSpec, _ciphertext )

    val expectedAddress = address( jsv )
    val pvtKeyAddress   = out.toPublicKey.toAddress

    if ( pvtKeyAddress != expectedAddress ) {
      throw new V3.Exception( s"Wallet is for address ${expectedAddress}, but decoded a private key for address '${pvtKeyAddress}'" )
    } else {
      out
    }
  }

  private def decodePrivateKey( cipher : Cipher, key : SecretKey, ivParameterSpec : IvParameterSpec, ciphertext : Array[Byte] ) : EthPrivateKey = {
    cipher.init(Cipher.DECRYPT_MODE, key, ivParameterSpec)
    EthPrivateKey( ByteSeqExact32( cipher.doFinal( ciphertext ) ) )
  }

  private def findKeyMac( jsv : JsValue, passphrase : String )( implicit provider : jce.Provider ) : KeyMac = {

    val _salt = salt( jsv )

    val _derivedKeyLength = dklen( jsv )

    kdf( jsv ) match {
      case "scrypt" => {
        val _iterationCount = scrypt.n( jsv )
        val _r              = scrypt.r( jsv )
        val _p              = scrypt.p( jsv )
        findSecretKeyScrypt( passphrase.getBytes( Codec.UTF8.charSet ), _salt, _iterationCount, _r, _p, _derivedKeyLength )( provider )
      }
      case "pbkdf2" => {
        val _iterationCount = pbkdf2.c( jsv )
        val _algoName       = pbkdf2.prfAsAlgoName( jsv )
        findSecretKeyPBE( _algoName, passphrase, _salt, _iterationCount, _derivedKeyLength )( provider )
      }
    }
  }

  private def findSecretKeyPBE( algoName : String, passphrase : String, salt : Array[Byte], iterationCount : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : KeyMac = {
    new KeyMac( findRawSecretKeyPBE( algoName, passphrase, salt, iterationCount, derivedKeyLength )( provider ) )
  }

  private def findRawSecretKeyPBE( algoName : String, passphrase : String, salt : Array[Byte], iterationCount : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : Array[Byte] = {
    /*
    // Grr.. bouncycastle JCE implementation doesn't yet support algo name "PBKDF2WithHmacSHA256" yet 

    val factory = SecretKeyFactory.getInstance( algoName, provider.name )
    val keySpec = new PBEKeySpec( passphrase.toCharArray, salt, iterationCount, derivedKeyLength )
    factory.generateSecret(keySpec).getEncoded()
    */

    jce.Provider.warnForbidUnavailableProvider( this, jce.Provider.BouncyCastle )( provider ) // we can't do this via JCE, using bouncycastle directly

    //import org.bouncycastle.crypto.digests.KeccakDigest
    import org.bouncycastle.crypto.digests.SHA256Digest
    import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
    import org.bouncycastle.crypto.params.KeyParameter

    // modified from http://stackoverflow.com/questions/22580853/reliable-implementation-of-pbkdf2-hmac-sha256-for-java
    val gen = new PKCS5S2ParametersGenerator(new SHA256Digest())
    gen.init(passphrase.getBytes("UTF-8"), salt, iterationCount)
    gen.generateDerivedParameters(derivedKeyLength * 8).asInstanceOf[KeyParameter].getKey()
  }

  private def findSecretKeyScrypt( passphrase : Array[Byte], salt : Array[Byte], iterationCount : Int, r : Int, p : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : KeyMac = {
    new KeyMac( findRawSecretKeyScrypt( passphrase, salt, iterationCount, r, p, derivedKeyLength )( provider ) )
  }

  private def findRawSecretKeyScrypt( passphrase : Array[Byte], salt : Array[Byte], iterationCount : Int, r : Int, p : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : Array[Byte] = {
    jce.Provider.warnForbidUnavailableProvider( this, jce.Provider.BouncyCastle )( provider ) // we can't do this via JCE, using bouncycastle directly

    import org.bouncycastle.crypto.generators.SCrypt

    SCrypt.generate( passphrase, salt, iterationCount, r, p, derivedKeyLength )
  }

  private final class KeyMac( rawKey : Array[Byte] ) {
    val key = new SecretKeySpec( rawKey.slice(0,16), MainCryptAlgoName )
    def mac( ciphertext : Array[Byte] ) = EthHash.hash(rawKey.slice(16,32) ++ ciphertext).bytes
  }

  private def address( jsv : JsValue ) : EthAddress = EthAddress( ByteSeqExact20( ( jsv \ "address" ).as[String].decodeHex ) )

  private def dklen( jsv : JsValue ) : Int = ( jsv \ "crypto" \ "kdfparams" \ "dklen" ).as[Int]

  private def ciphertext( jsv : JsValue ) : Array[Byte] = assertByteArray( jsv \ "crypto" \ "ciphertext" )

  private def cipher( jsv : JsValue)( implicit provider : jce.Provider ) : Cipher = ( jsv \ "crypto" \ "cipher" ).as[String] match {
    case MainCryptAlgoWalletName => Cipher.getInstance( MainCryptAlgoNameFull, provider.name )
    case other                   => throw new V3.Exception( s"Unexpected cipher: ${other}" )
  }

  private def kdf( jsv : JsValue ) : String = ( jsv \ "crypto" \ "kdf" ).as[String]

  private def mac( jsv : JsValue ) = assertByteArray( jsv \ "crypto" \ "mac" ).toImmutableSeq // we will do an equality check, so let's not use arrays

  private def iv( jsv : JsValue )   : Array[Byte] = assertByteArray( jsv \ "crypto" \ "cipherparams" \ "iv" )
  private def salt( jsv : JsValue ) : Array[Byte] = assertByteArray( jsv \ "crypto" \ "kdfparams" \ "salt" )

  private def version( jsv : JsValue ) : Int = ( jsv \ "version" ).as[Int]

  private def assertByteArray( jslr : JsLookupResult ) : Array[Byte] = jslr.as[String].decodeHex

  private final object scrypt {
    def n( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "n").as[Int]
    def r( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "r").as[Int]
    def p( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "p").as[Int]
  }

  private final object pbkdf2 {
    def c( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "c").as[Int]

    def prfAsAlgoName( jsv : JsValue ) : String = {
      ( jsv \ "crypto" \ "kdfparams" \ "prf" ).as[String] match {
        case "hmac-sha256" => Pbkdf2AlgoName
        case other         => throw new V3.Exception( s"Unexpected prf value '${other}'" )
      }
    }
  }
}
case class V3( jso : JsObject ) {
  def address = V3.address( jso )
  def write( os : OutputStream ) : Unit = {
    val bytes = Json.stringify( jso ).getBytes( Codec.UTF8.charSet )
    os.write( bytes )
  }
}


