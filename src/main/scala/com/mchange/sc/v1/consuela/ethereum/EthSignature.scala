package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

import specification.Set.SignatureV;
import specification.Set.SignatureR;
import specification.Set.SignatureS;

case class EthSignature( val v : Byte, val r : BigInt, val s : BigInt ) {
  require( (v elem_!: SignatureV) && (r elem_!: SignatureR) && (s elem_!: SignatureS) );

  def bytesWereSigned( bytes : Array[Byte] ) : Option[EthPublicKey] = {
    crypto.secp256k1.BouncyCastlePublicKeyComputer.recoverPublicKeyBytesV( v, r.bigInteger, s.bigInteger, bytes ).map( EthPublicKey(_) )
  }

  def hashWasSigned( hash : EthHash )        : Option[EthPublicKey] = bytesWereSigned( hash.toByteArray );
  def hashWasSigned( bytes : Array[Byte] )   : Option[EthPublicKey] = hashWasSigned( EthHash.hash( bytes ) );

  lazy val exportByteSeq = v.toByte +: Vector( (r.unsignedBytes(32) ++ s.unsignedBytes(32)) : _* ); 
}
