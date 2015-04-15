package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.jce;
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
}

final class EthPrivateKey private( protected val _bytes : Array[Byte] ) extends ByteArrayValue with ByteArrayValue.UnsignedBigIntegral {
  require( _bytes.length == EthPrivateKey.ByteLength );

  def s = this.toBigInt;

  def sign( document : Array[Byte] )( implicit provider : jce.Provider ) : EthSignature = { 
    import crypto.secp256k1._;
    val docHash = EthHash.hash( document ); // ethereum signatures are (as usual) of hashes, not documents directly
    val docHashBytes = docHash.toByteArray
    signature( this.toBigInteger, docHashBytes )( provider ) match {
      case Left( bytes ) => throw new UnexpectedSignatureFormatException( bytes.hex );
      case Right( Signature( r, s, Some( v ) ) ) => EthSignature( v, r, s );
      case Right( Signature( r, s, None ) )      => {
        val mbRecovered = BouncyCastlePublicKeyComputer.recoverPublicKeyAndV( r, s, docHashBytes );
        mbRecovered.fold( throw new EthereumException( s"Could find only partial signature [ v -> ???, r -> ${r}, s -> ${s} ]" ) ) { recovered =>
          EthSignature( recovered.v.toByte, r, s )
        }
      }
    }
  }

  lazy val toPublicKey = EthPublicKey( this );
}

