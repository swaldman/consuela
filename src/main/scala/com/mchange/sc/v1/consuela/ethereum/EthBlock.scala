package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Set.{Unsigned, Unsigned2048, ByteSeq1024};

import scala.collection.immutable.Seq;

object EthBlock {
  case class Header( 
    parentHash      : EthHash, 
    ommersHash      : EthHash, 
    coinbase        : EthAddress, 
    stateRoot       : EthHash, 
    transactionRoot : EthHash, 
    receiptsRoot    : EthHash,
    logsBloom       : BigInt,  /* XXX: Is EthHash really the right datatype for this? I'd better figure out what it is. */
    difficulty      : BigInt,
    number          : BigInt,
    gasLimit        : BigInt,
    gasUsed         : BigInt,
    timestamp       : BigInt,
    extraData       : Seq[Byte],
    nonce           : EthHash
  ) {
    require(
      ( logsBloom  elem_!: Unsigned2048 ) &&
      ( difficulty elem_!: Unsigned     ) &&
      ( number     elem_!: Unsigned     ) &&
      ( gasLimit   elem_!: Unsigned     ) &&
      ( gasUsed    elem_!: Unsigned     ) &&
      ( timestamp  elem_!: Unsigned     ) &&
      ( extraData  elem_!: ByteSeq1024  )
    )
  }
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
