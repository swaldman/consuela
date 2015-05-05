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

    def childGasLimitRange( parent : Header ) : ( Unsigned256, Unsigned256 ) = {
      val ( low, high ) = _childGasLimitRange( parent.gasLimit.widen );
      ( Unsigned256( low ), Unsigned256( high ) )
    }

    private def _childGasLimitRange( parentGasLimit : BigInt ) : ( BigInt, BigInt ) = {
      val radius = parentGasLimit / 1024;
      ( parentGasLimit - radius, parentGasLimit + radius )
    }

    private def _inGasLimitRange( childGasLimit : BigInt, parentGasLimit : BigInt ) : Boolean = {
      val ( low, high ) = _childGasLimitRange( parentGasLimit );
      childGasLimit > low && childGasLimit < high
    }

    private def _legalGasLimit( childGasLimit : BigInt, parentGasLimit : BigInt ) : Boolean = {
      _inGasLimitRange( childGasLimit, parentGasLimit ) && childGasLimit >= 125000
    }

    def isValidChildOfParent( putativeChild : Header, putativeParent : Header ) : Boolean = {
      def numberMatches     = putativeChild.number.widen == putativeParent.number.widen + 1;
      def hashMatches       = EthHash.hash( RLP.encode( putativeParent ) ) == putativeChild.parentHash;
      def difficultyMatches = childDifficulty( putativeChild.timestamp, putativeParent ) == putativeParent.difficulty;
      def legalGasLimit     = _legalGasLimit( putativeChild.gasLimit.widen, putativeParent.gasLimit.widen );

      numberMatches && hashMatches && difficultyMatches && legalGasLimit
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
    def isValidChild( putativeParent : Header ) : Boolean = Header.isValidChildOfParent( this, putativeParent );
  }
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
