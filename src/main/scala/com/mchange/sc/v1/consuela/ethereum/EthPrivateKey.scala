package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.crypto;
import com.mchange.sc.v1.consuela.Implicits._;

import scala.collection._;

object EthPrivateKey {
  val ByteLength = crypto.secp256k1.ValueByteLength;

  def apply( bytes : Seq[Byte] ) : EthPrivateKey = {
    bytes match {
      case safe : immutable.Seq[Byte] => EthPrivateKey( safe );
      case unsafe => EthPrivateKey( immutable.Seq( unsafe : _* ) );
    }
  }
  def apply( bytes : Array[Byte] ) : EthPrivateKey = EthPrivateKey( immutable.Seq( bytes : _* ) );
  def apply( bigInt : BigInt ) : EthPrivateKey     = this.apply( bigInt.unsignedBytes( ByteLength ) );

  private val HashSalt = -1507782977; // a randomly generated Int
}

final class EthPrivateKey private( val bytes : immutable.Seq[Byte] ) {
  require( bytes.length == EthPrivateKey.ByteLength );

  lazy val toByteArray  = bytes.toArray;
  lazy val toBigInteger = new java.math.BigInteger(1, bytes.toArray);
  lazy val toBigInt     = BigInt( this.toBigInteger );

  override def toString() : String = s"EthPrivateKey [${bytes.hex}]";

  override def equals( o : Any ) : Boolean = {
    o match {
      case epk : EthPrivateKey => this.bytes == epk.bytes;
      case _ => false;
    }
  }
  override def hashCode() : Int = bytes.hashCode() ^ EthPrivateKey.HashSalt; 
}

