package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.crypto;
import com.mchange.sc.v1.consuela.Implicits._;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import java.security.SecureRandom;

object EthPrivateKey {
  val ByteLength = crypto.secp256k1.ValueByteLength;

  def apply( bytes  : Seq[Byte] )    : EthPrivateKey = new EthPrivateKey( bytes.toArray );
  def apply( bytes  : Array[Byte] )  : EthPrivateKey = new EthPrivateKey( bytes.clone() );
  def apply( bigInt : BigInt )       : EthPrivateKey = new EthPrivateKey( bigInt.unsignedBytes( ByteLength ) );
  def apply( random : SecureRandom ) : EthPrivateKey = {
    val bytes = Array.ofDim[Byte](ByteLength);
    random.nextBytes( bytes );
    new EthPrivateKey( bytes );
  }

  private val HashSalt = -1507782977; // a randomly generated Int
}

final class EthPrivateKey private( protected val _bytes : Array[Byte] ) extends ByteArrayValue with ByteArrayValue.UnsignedBigIntegral {
  require( _bytes.length == EthPrivateKey.ByteLength );
}

