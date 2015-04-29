package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned,Unsigned2048,ByteSeqMax1024,ByteSeqExact8};

import scala.collection.immutable.Seq;

object EthBlock {
  case class Header( 
    parentHash      : EthHash, 
    ommersHash      : EthHash, 
    coinbase        : EthAddress, 
    stateRoot       : EthHash, 
    transactionRoot : EthHash, 
    receiptsRoot    : EthHash,
    logsBloom       : Unsigned2048,  
    difficulty      : Unsigned,
    number          : Unsigned,
    gasLimit        : Unsigned,
    gasUsed         : Unsigned,
    timestamp       : Unsigned,
    extraData       : ByteSeqMax1024,
    mixHash         : EthHash,
    nonce           : ByteSeqExact8
  )
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
