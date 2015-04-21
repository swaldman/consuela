package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

import scala.collection._;

import specification.Set.SignatureV;
import specification.Set.SignatureR;
import specification.Set.SignatureS;

case class EthSignature( val v : Byte, val r : BigInt, val s : BigInt ) {
  require( (v elem_!: SignatureV) && (r elem_!: SignatureR) && (s elem_!: SignatureS) );

  private def rawBytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.recoverPublicKeyBytesV( v, r.bigInteger, s.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  private def ethHashWasSigned( hash : EthHash )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray );
  private def ethHashWasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) );

  // default
  def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = this.ethHashWasSigned( document );

  lazy val exportByteSeq = v.toByte +: Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* ); 
}
