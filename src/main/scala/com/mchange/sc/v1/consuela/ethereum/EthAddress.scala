package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable};

import com.mchange.sc.v1.consuela.crypto;
import com.mchange.sc.v1.consuela.Implicits._;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;
import com.mchange.sc.v1.consuela.ethereum.util.EthByteArrayValue;

import java.util.Arrays;

object EthAddress extends RLPSerializable.Companion[EthAddress] {
  val ByteLength = 20;
  def apply( bytes : Seq[Byte] )     : EthAddress = new EthAddress( bytes.toArray );
  def apply( bytes : Array[Byte] )   : EthAddress = new EthAddress( bytes.clone() );
  def apply( pub   : EthPublicKey )  : EthAddress = new EthAddress( this.computeBytes( pub ) );

  def computeBytes( pub : EthPublicKey ) : Array[Byte] = EthHash.hash(pub.toByteArray).toByteArray.drop(12);

  // RLPSerializable.Companion stuff
  def toRLPEncodable( address : EthAddress )              : RLP.Encodable = RLP.Encodable.ByteSeq( address.bytes );
  def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : EthAddress    = encodable match { case RLP.Encodable.ByteSeq( bytes ) => EthAddress( bytes ) }
}
final class EthAddress private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue with EthByteArrayValue.Nibbly with RLPSerializable[EthAddress] {
  require( _bytes.length == EthAddress.ByteLength );

  protected val companion = EthAddress;

  def matches( pub : EthPublicKey ) : Boolean = Arrays.equals( _bytes, EthAddress.computeBytes( pub ) );
}
