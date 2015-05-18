package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned64,Unsigned256,Unsigned2048,ByteSeqMax1024};

import com.mchange.sc.v1.consuela.ethereum.{EthBlockDetails => Details}

import scala.collection.immutable.Seq;

object EthBlock {
  val Genesis : EthBlock = Details.GenesisBlock;

  final object Header {
    def isValidChildOfParent( putativeChild : Header, putativeParent : Header ) : Boolean = Details.Header.isValidChildOfParent( putativeChild, putativeParent );
  }
  final case class Header( 
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
    mixHash         : EthHash         = AllZeroesEthHash,
    nonce           : Unsigned64      = Unsigned64(0)
  ) {
    def isValidChild( putativeParent : Header ) : Boolean = Header.isValidChildOfParent( this, putativeParent );
  }
}
final case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
