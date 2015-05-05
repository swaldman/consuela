package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned256,Unsigned2048,ByteSeqMax1024,ByteSeqExact8};

import scala.collection.immutable.Seq;

object EthBlock {
  case class Header( 
    parentHash      : EthHash, 
    ommersHash      : EthHash, 
    coinbase        : EthAddress, 
    stateRoot       : EthHash, 
    transactionRoot : EthHash, 
    receiptsRoot    : EthHash,
    logsBloom       : EthLogBloom,  
    difficulty      : Unsigned256,
    number          : Unsigned256,
    gasLimit        : Unsigned256,
    gasUsed         : Unsigned256,
    timestamp       : Unsigned256,
    extraData       : ByteSeqMax1024,
    mixHash         : EthHash,
    nonce           : ByteSeqExact8
  )
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
