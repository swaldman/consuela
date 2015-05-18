package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;

import scala.collection._;

import specification.Types.{SignatureV, SignatureR, SignatureS};

final case class EthSignature( val v : SignatureV, val r : SignatureR, val s : SignatureS ) {

  private def rawBytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.recoverPublicKeyBytesV( v.widen, r.widen.bigInteger, s.widen.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  private def ethHashWasSigned( hash : EthHash )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray );
  private def ethHashWasSigned( document : Array[Byte] ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) );

  // default
  def wasSigned( document : Array[Byte] ) : Option[EthPublicKey] = this.ethHashWasSigned( document );

  lazy val exportByteSeq = v.widen +: Vector( (r.widen.unsignedBytes(32) ++ s.widen.unsignedBytes(32)) : _* ); 
}
