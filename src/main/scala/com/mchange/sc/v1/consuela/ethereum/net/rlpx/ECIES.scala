package com.mchange.sc.v1.consuela.ethereum.net.rlpx;

import java.security.SecureRandom;
import javax.crypto.Cipher;

import com.mchange.sc.v1.consuela.{crypto,MainProvider};
import com.mchange.sc.v1.consuela.ethereum.{EthPrivateKey,EthPublicKey};

object ECIES {
  val Default : ECIES = new ECIES( () => new SecureRandom )

  val EmptyByteArray = Array.ofDim[Byte](0)
}
class ECIES( val randomFactory : () => SecureRandom ) {
  val CipherName = "ECIESwithAES"

  def encrypt( pubkey : EthPublicKey, plaintext : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {

    val ecPublicKey = crypto.secp256k1.jce_public_key_from_XY( pubkey.x.bigInteger, pubkey.y.bigInteger )( provider )
    val cipher = Cipher.getInstance( CipherName, provider.name )
    cipher.init( Cipher.ENCRYPT_MODE, ecPublicKey, randomFactory() )

    // hopefully unnecessary... UNHARDCODE IF IT BECOMES NECESSARY
    //val p = new org.bouncycastle.jce.spec.IESParameterSpec( ECIES.EmptyByteArray, ECIES.EmptyByteArray, 128 )
    //cipher.init( Cipher.ENCRYPT_MODE, ecPublicKey, p, randomFactory() )

    cipher.doFinal( plaintext, 0, plaintext.length )
  }
  def decrypt( pvtkey : EthPrivateKey, ciphertext : Array[Byte] ) : Array[Byte] = {
    ???
  }
}
