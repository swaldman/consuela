package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.crypto;

import specification.Types.{SignatureV, SignatureR, SignatureS}

import com.mchange.sc.v1.consuela.util.ByteSeqValue;

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import java.security.SecureRandom;

object EthPrivateKey {
  val ByteLength = crypto.secp256k1.ValueByteLength;

  def apply( bytes  : Seq[Byte] )    : EthPrivateKey = {
    bytes match {
      case iasb : ImmutableArraySeq.Byte => new EthPrivateKey( iasb );
      case other                         => new EthPrivateKey( ImmutableArraySeq.Byte( other.toArray ) );
    }
  }
  def apply( bytes  : Array[Byte] )  : EthPrivateKey = new EthPrivateKey( ImmutableArraySeq.Byte( bytes ) );
  def apply( bigInt : BigInt )       : EthPrivateKey = new EthPrivateKey( ImmutableArraySeq.Byte.createNoCopy( bigInt.unsignedBytes( ByteLength ) ) );
  def apply( random : SecureRandom ) : EthPrivateKey = {
    val bytes = Array.ofDim[Byte](ByteLength);
    random.nextBytes( bytes );
    new EthPrivateKey( ImmutableArraySeq.Byte.createNoCopy( bytes ) )
  }
}

final class EthPrivateKey private( val bytes : ImmutableArraySeq.Byte ) extends ByteSeqValue with ByteSeqValue.UnsignedBigIntegral {
  require( bytes.length == EthPrivateKey.ByteLength );

  def s = this.toBigInt;

  private def signRawBytes( rawBytes : Array[Byte] ) : EthSignature = { 
    import crypto.secp256k1._;
    signature( this.toBigInteger, rawBytes ) match {
      case Left( bytes ) => throw new UnexpectedSignatureFormatException( bytes.hex );
      case Right( Signature( r, s, Some( v ) ) ) => EthSignature( SignatureV( v ), SignatureR( BigInt(r) ), SignatureS( BigInt(s) ) );
      case Right( Signature( r, s, None ) )      => {
        val mbRecovered = recoverPublicKeyAndV( r, s, rawBytes );
        mbRecovered.fold( throw new EthereumException( s"Could find only partial signature [ v -> ???, r -> ${r}, s -> ${s} ]" ) ) { recovered =>
          EthSignature( SignatureV( recovered.v ), SignatureR( BigInt( r ) ), SignatureS( BigInt( s ) ) )
        }
      }
    }
  }
  private def signEthHash( hash : EthHash ) = this.signRawBytes( hash.toByteArray );
  private def signEthHash( document : Array[Byte] ) : EthSignature = this.signEthHash( EthHash.hash( document ) );

  // default signing scheme
  def sign( document : Array[Byte] ) : EthSignature = this.signEthHash( document );

  lazy val toPublicKey = EthPublicKey( this );
}

