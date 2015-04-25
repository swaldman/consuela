package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.bloom.Bloom;

import scala.collection._;

import com.mchange.lang.LongUtils.longFromByteArray;

object EthBloom {
  def indicesFromEthHash( hash : EthHash ) : immutable.Set[Int] = {
    val bottom8 : Long = longFromByteArray( hash.toByteArray, EthHash.HashLength - 8 );
    val mask    : Long = 0x07FF; // the eleven least-significant-bits set

    def uint11( offset : Int ) : Int = ((bottom8 & (mask << offset) >>> offset)).toInt
    immutable.Set( uint11(0), uint11(16), uint11(32) )
  }

  class EthBloomDefinition[T]( xform : T => EthHash ) extends Bloom.Definition[T] {
    def indices( t : T ) : immutable.Set[Int] = indicesFromEthHash( xform(t) ); // no need to mod, since these are 11 bit values

    val NumHashes : Int = 3
    val BitLength : Int = 1 << 11; // 1 << 11 == 2048
  }
}
