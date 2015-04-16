package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

import specification.Set.SignatureV;
import specification.Set.SignatureR;
import specification.Set.SignatureS;

case class EthSignature( val v : Byte, val r : BigInt, val s : BigInt ) {
  require( (v elem_!: SignatureV) && (r elem_!: SignatureR) && (s elem_!: SignatureS) );

  private def rawBytesWereSigned( bytes : Array[Byte] )( implicit provider : jce.Provider ) : Option[EthPublicKey] = {
    //XXX TODO: Do the best that can be done to make this more sensitive to provider
    jce.Provider.warnForbidUnconfiguredUseOfBouncyCastle( this )( provider )

    crypto.secp256k1.BouncyCastlePublicKeyComputer.recoverPublicKeyBytesV( v, r.bigInteger, s.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  private def ethHashWasSigned( hash : EthHash )( implicit provider : jce.Provider )         : Option[EthPublicKey] = rawBytesWereSigned( hash.toByteArray )( provider );
  private def ethHashWasSigned( document : Array[Byte] )( implicit provider : jce.Provider ) : Option[EthPublicKey] = ethHashWasSigned( EthHash.hash( document ) )( provider );

  // default
  def wasSigned( document : Array[Byte] )( implicit provider : jce.Provider ) : Option[EthPublicKey] = this.ethHashWasSigned( document )( provider );

  lazy val exportByteSeq = v.toByte +: Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* ); 
}
