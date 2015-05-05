package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned256,Unsigned2048,ByteSeqMax1024,ByteSeqExact8};

import scala.collection.immutable.Seq;

object EthBlock {
  object Header {
    // from yellowpaper 4.3.4
    val GenesisDifficulty  = Unsigned256( 131072 ); 
    val LateChildThreshold = 8; // seconds

    def childDifficulty( childTimestamp : Unsigned256, parent : Header ) : Unsigned256 = {
      /*
       *  we will adjust the difficulty by increment
       *  we will adjust it up if our new timestamp is with 8 secs (LateChildThreshold) of the last, down otherwise
       *  we never increment below GenesisDifficulty
       */
      def newDifficulty( childTimestamp : BigInt, parentDifficulty : BigInt, parentTimestamp : BigInt ) : BigInt =  {
        val increment   = parentDifficulty / 2048;
        val incremented = parentDifficulty + ( if ( childTimestamp < parentTimestamp + LateChildThreshold ) increment else -increment );
        _GenesisDifficulty.max( incremented );
      }

      Unsigned256( newDifficulty( childTimestamp.widen, parent.difficulty.widen, parent.timestamp.widen ) );
    }

    def isChildOfParent( putativeChild : Header, putativeParent : Header ) : Boolean = {
      def numberMatches = putativeChild.number.widen == putativeParent.number.widen + 1;
      def hashMatches   = EthHash.hash( RLP.encode( putativeParent ) ) == putativeChild.parentHash;

      numberMatches && hashMatches
    }

    private val _GenesisDifficulty = GenesisDifficulty.widen;
  }
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
  ) {
    def isParent( putativeParent : Header ) : Boolean = Header.isChildOfParent( this, putativeParent );
  }
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
