package com.mchange.sc.v1.consuela.ethereum;

import scala.collection._;

import specification.Types.ByteSeqExact4;

object EthLogEntry {
  case class Topic( bytes : immutable.Seq[Byte] ) {
    require( bytes elem_!: ByteSeqExact4 )
  }
}
case class EthLogEntry( address : EthAddress, topics : immutable.Seq[EthLogEntry.Topic], data : immutable.Seq[Byte] );
