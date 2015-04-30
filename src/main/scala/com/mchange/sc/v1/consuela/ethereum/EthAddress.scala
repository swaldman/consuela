package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializing};

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.crypto;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;
import com.mchange.sc.v1.consuela.ethereum.util.EthByteArrayValue;
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20;

import java.util.Arrays;

// TODO: Simplify, reimplement as case class accepting ByteSeqExact20 (someday, maybe)

object EthAddress {
  val ByteLength = 20;
  def apply( bytes : Seq[Byte] )      : EthAddress = new EthAddress( bytes.toArray );
  def apply( bytes : Array[Byte] )    : EthAddress = new EthAddress( bytes.clone() );
  def apply( pub   : EthPublicKey )   : EthAddress = new EthAddress( this.computeBytes( pub ) );

  def apply( bytes : ByteSeqExact20 ) : EthAddress = this.apply( bytes.widen );

  def computeBytes( pub : EthPublicKey ) : Array[Byte] = EthHash.hash(pub.toByteArray).toByteArray.drop(12);
}
final class EthAddress private ( protected val _bytes : Array[Byte] ) extends ByteArrayValue with EthByteArrayValue.Nibbly {

  require( _bytes.length == EthAddress.ByteLength );

  lazy val toByteSeqExact20 : ByteSeqExact20 = ByteSeqExact20( _bytes )

  def matches( pub : EthPublicKey ) : Boolean = Arrays.equals( _bytes, EthAddress.computeBytes( pub ) );
}
