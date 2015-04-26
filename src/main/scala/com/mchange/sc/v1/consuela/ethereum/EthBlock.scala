package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Set._;

import scala.collection.immutable.Seq;

object EthBlock {
  case class Header( 
    parentHash      : EthHash, 
    ommersHash      : EthHash, 
    coinbase        : EthAddress, 
    stateRoot       : EthHash, 
    transactionRoot : EthHash, 
    receiptsRoot    : EthHash,
    logsBloom       : BigInt,  
    difficulty      : BigInt,
    number          : BigInt,
    gasLimit        : BigInt,
    gasUsed         : BigInt,
    timestamp       : BigInt,
    extraData       : Seq[Byte],
    mixHash         : EthHash,
    nonce           : Seq[Byte]
  ) {
    require(
      ( logsBloom  elem_!: Unsigned2048   ) &&
      ( difficulty elem_!: Unsigned       ) &&
      ( number     elem_!: Unsigned       ) &&
      ( gasLimit   elem_!: Unsigned       ) &&
      ( gasUsed    elem_!: Unsigned       ) &&
      ( timestamp  elem_!: Unsigned       ) &&
      ( extraData  elem_!: ByteSeqMax1024 ) &&
      ( nonce      elem_!: ByteSeqExact8  )
    )
  }
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
