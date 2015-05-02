package com.mchange.sc.v1.consuela.ethereum;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

object EthKeyAddressProperties extends Properties("Ethereum Keys and Addresses") {

  lazy val keyEntropy = new java.security.SecureRandom;

  lazy val priv = EthPrivateKey( keyEntropy );
  lazy val pub  = priv.toPublicKey;
  lazy val addr = pub.toAddress;

  property("EthPrivateKey length is 32 bytes (256 bits)") = Prop( priv.bytes.length == 32 );
  property("EthPublicKey length is 64 bytes (512 bits)")  = Prop( pub.bytes.length == 64 );
  property("EthAddress length is 20 bytes")               = Prop( addr.bytes.length == 20 );

  property("The Public Key verifies what the private key signs") = Prop.forAll { (message : Array[Byte]) => pub.verify( message, priv.sign( message ) ) };
}

