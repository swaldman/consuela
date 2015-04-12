package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.crypto;
import com.mchange.sc.v1.consuela.Implicits._;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

import java.util.Arrays;

object EthAddress {
  val ByteLength = 20;
  def apply( bytes : Seq[Byte] )     : EthAddress = new EthAddress( bytes.toArray );
  def apply( bytes : Array[Byte] )   : EthAddress = new EthAddress( bytes.clone() );
  def apply( pub   : EthPublicKey )  : EthAddress = new EthAddress( this.computeBytes( pub ) );

  def computeBytes( pub : EthPublicKey ) : Array[Byte] = EthHash.hash(pub.toByteArray).toByteArray.drop(12);
}
final class EthAddress private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue {
  require( _bytes.length == EthAddress.ByteLength );

  def matches( pub : EthPublicKey ) : Boolean = Arrays.equals( _bytes, EthAddress.computeBytes( pub ) );
}
