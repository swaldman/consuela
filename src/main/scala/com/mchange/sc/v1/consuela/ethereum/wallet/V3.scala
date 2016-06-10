package com.mchange.sc.v1.consuela.ethereum.wallet

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.EthPrivateKey
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact32

import play.api.libs.json._

import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}

import scala.io.Codec

object V3 {
  class WalletException( msg : String ) extends Exception( msg )

  // taking a lot of this from https://github.com/ethereumjs/ethereumjs-wallet#remarks-about-tov3

  private val Pbkdf2AlgoName = "PBKDF2WithHmacSHA256"

  private val MainKeyCryptAlgoName = "AES"

  def decodePrivateKey( jsv : JsValue, passphrase : String )( implicit provider : jce.Provider ) : EthPrivateKey = {
    val _cipher          = cipher( jsv )( provider )
    val _key             = findKey( jsv, passphrase )( provider )
    val _ivParameterSpec = new IvParameterSpec( iv(jsv) )
    val _ciphertext      = ciphertext( jsv )
    decodePrivateKey( _cipher, _key, _ivParameterSpec, _ciphertext )
  }

  private def decodePrivateKey( cipher : Cipher, key : SecretKey, ivParameterSpec : IvParameterSpec, ciphertext : Array[Byte] ) : EthPrivateKey = {
    cipher.init(Cipher.DECRYPT_MODE, key, ivParameterSpec)
    EthPrivateKey( ByteSeqExact32( cipher.doFinal( ciphertext ) ) )
  }

  private def findKey( jsv : JsValue, passphrase : String )( implicit provider : jce.Provider ) : SecretKey = {

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

  private def findSecretKeyPBE( algoName : String, passphrase : String, salt : Array[Byte], iterationCount : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : SecretKey = {
    val factory = SecretKeyFactory.getInstance( algoName, provider.name )
    val keySpec = new PBEKeySpec( passphrase.toCharArray, salt, iterationCount, derivedKeyLength )
    factory.generateSecret(keySpec);
  }

  private def findSecretKeyScrypt( passphrase : Array[Byte], salt : Array[Byte], iterationCount : Int, r : Int, p : Int, derivedKeyLength : Int )( implicit provider : jce.Provider ) : SecretKeySpec = {
    jce.Provider.warnForbidUnavailableProvider( this, jce.Provider.BouncyCastle )( provider ) // we can't do this via JCE, using bouncycastle directly

    import org.bouncycastle.crypto.generators.SCrypt

    val raw = SCrypt.generate( passphrase, salt, iterationCount, r, p, derivedKeyLength )
    new SecretKeySpec( raw, MainKeyCryptAlgoName )
  }

  private def dklen( jsv : JsValue )      : Int = ( jsv \ "crypto" \ "kdfparams" \ "dklen" ).as[Int]

  private def ciphertext( jsv : JsValue ) : Array[Byte] = assertByteArray( jsv \ "crypto" \ "ciphertext" )

  private def cipher( jsv : JsValue)( implicit provider : jce.Provider ) : Cipher = ( jsv \ "crypto" \ "cipher" ).as[String] match {
    case "aes-128-ctr" => Cipher.getInstance( "AES/CTR/NoPadding", provider.name )
    case other         => throw new WalletException( s"Unexpected cipher: ${other}" )
  }

  private def kdf( jsv : JsValue ) : String = ( jsv \ "crypto" \ "kdf" ).as[String]

  private def iv( jsv : JsValue )         : Array[Byte] = assertByteArray( jsv \ "crypto" \ "cipherparams" \ "iv" )
  private def salt( jsv : JsValue )       : Array[Byte] = assertByteArray( jsv \ "crypto" \ "kdfparams" \ "salt" )

  private def assertByteArray( jslr : JsLookupResult ) : Array[Byte] = jslr.as[String].decodeHex

  private final object scrypt {
    def n( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "n").as[Int]
    def r( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "r").as[Int]
    def p( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "p").as[Int]
  }

  private final object pbkdf2 {
    def c( jsv : JsValue ) : Int = (jsv \ "crypto" \ "kdfparams" \ "c").as[Int]

    def prfAsAlgoName( jsv : JsValue ) : String = {
      jsv.as[String] match {
        case "hmac-sha256" => Pbkdf2AlgoName
        case other         => throw new WalletException( s"Unexpected prf value '${other}'" )
      }
    }
  }
}


/*
  final object Cipher {
    final object 
  }
  abstract class Cipher[T <: CipherParams]( name : String ) {
    def create( params : T ) : javax.crypto.Cipher;
  }

  final object Kdf {
    final object scrypt {
    }
  }
  sealed trait Kdf {
    val name : String
  }


  object CipherParams {
    case class Aes128Ctr( iv : ByteSeqExact16 ) extends CipherParams
  }
  sealed trait CipherParams;

  case class Crypto( cipher : Cipher, ciphertext : immutable.Seq[Byte], cipherparams : CipherParams, kdf : Kdf, kdfparams : KdfParams, mac : immutable.Seq[Byte] )
  case class Wallet( address : EthAddress, crypto : Crypto, id : String, version : Int )
 */ 

