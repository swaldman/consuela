package com.mchange.sc.v1.consuela.ethereum;

import scala.collection._;

import specification.Types.{ByteSeqExact4 => Topic}

case class EthLogEntry( address : EthAddress, topics : immutable.Seq[Topic], data : immutable.Seq[Byte] );
