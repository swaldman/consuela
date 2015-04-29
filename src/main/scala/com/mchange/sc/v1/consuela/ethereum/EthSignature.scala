package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

import scala.collection._;

import specification.Types.{SignatureV, SignatureR, SignatureS};

case class EthSignature( val v : SignatureV, val r : SignatureR, val s : SignatureS ) {

  private def rawBytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.recoverPublicKeyBytesV( v.value, r.value.bigInteger, s.value.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  private def ethHashWasSigned( hash : EthHash )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray );
  private def ethHashWasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) );

  // default
  def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = this.ethHashWasSigned( document );

  lazy val exportByteSeq = v.value +: Vector( (r.value.unsignedBytes(32) ++ s.value.unsignedBytes(32)) : _* ); 
}
