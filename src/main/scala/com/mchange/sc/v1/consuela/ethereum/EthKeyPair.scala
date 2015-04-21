package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.crypto;

import java.security.SecureRandom;

object EthKeyPair {
  def apply( rand : SecureRandom )  : EthKeyPair = {
    val bytesPair = crypto.secp256k1.generate_bytes_keypair( rand );
    EthKeyPair( EthPrivateKey( bytesPair._1 ), EthPublicKey( bytesPair._2 ) )
  }
  def apply( priv : EthPrivateKey ) : EthKeyPair = EthKeyPair( priv, EthPublicKey( priv ) );
}
case class EthKeyPair( val Private : EthPrivateKey, val Public : EthPublicKey ) {
  lazy val Address = Public.toAddress;
}
