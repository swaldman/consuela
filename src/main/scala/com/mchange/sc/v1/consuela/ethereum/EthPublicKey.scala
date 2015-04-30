package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.crypto;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import java.util.Arrays;

object EthPublicKey {
  val ByteLength = 2 * crypto.secp256k1.ValueByteLength;

  def apply( bytes : Seq[Byte] )     : EthPublicKey = new EthPublicKey( bytes.toArray );
  def apply( bytes : Array[Byte] )   : EthPublicKey = new EthPublicKey( bytes.clone() );
  def apply( priv  : EthPrivateKey ) : EthPublicKey = new EthPublicKey( this.computeBytes( priv ) );

  def computeBytes( priv : EthPrivateKey ) : Array[Byte] = crypto.secp256k1.computePublicKeyBytes( priv.toBigInteger )
}
final class EthPublicKey private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue {
  require( _bytes.length == EthPublicKey.ByteLength );

  private lazy val (_xBytes, _yBytes) = _bytes.splitAt( crypto.secp256k1.ValueByteLength );
  lazy val x = BigInt( new java.math.BigInteger( 1, _xBytes ) );
  lazy val y = BigInt( new java.math.BigInteger( 1, _yBytes ) );

  lazy val toAddress = EthAddress( this );

  def matches( priv : EthPrivateKey ) : Boolean = Arrays.equals( _bytes, EthPublicKey.computeBytes( priv ) );

  private def verifyRawBytes( rawBytes : Array[Byte], signature : EthSignature ) : Boolean = {
    val _signature = crypto.secp256k1.Signature( signature.r.widen.bigInteger, signature.s.widen.bigInteger )
    crypto.secp256k1.verifySignature( rawBytes, _signature, this.x.bigInteger, this.y.bigInteger )
  }
  private def verifyEthHash( hash : EthHash, signature : EthSignature ) : Boolean = this.verifyRawBytes( hash.toByteArray, signature );

  private def verifyEthHash( document : Array[Byte], signature : EthSignature ) : Boolean = {
    this.verifyEthHash( EthHash.hash( document ), signature );
  }

  // default scheme
  def verify( document : Array[Byte], signature : EthSignature ) : Boolean = this.verifyEthHash( document, signature );
}


