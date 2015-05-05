package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned64,Unsigned256,Unsigned2048,ByteSeqMax1024};

import scala.collection.immutable.Seq;

object EthBlock {

  val Genesis = {
    import com.mchange.sc.v1.consuela._;

    // header state taken from ethereum/tests/BlockTests/bcTotalDifficultyTest.json 2015-05-05
    val header = Header(
      parentHash      = AllZerosEthHash,
      ommersHash      = EthHash.withBytes("0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347".decodeHex),
      coinbase        = EthAddress( "0x8888f1f195afa192cfee860698584c030f4c9db1".decodeHex ),
      stateRoot       = EthHash.withBytes( "0x7dba07d6b448a186e9612e5f737d1c909dce473e53199901a302c00646d523c1".decodeHex ),
      transactionRoot = EthHash.withBytes( "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421".decodeHex ),
      receiptsRoot    = EthHash.withBytes( "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421".decodeHex ),
      logsBloom       = EthLogBloom.empty,
      difficulty      = Unsigned256( 0x020000 ),
      number          = Unsigned256( 0 ),
      gasLimit        = Unsigned256( 0x2fefd8 ),
      gasUsed         = Unsigned256( 0 ),
      timestamp       = Unsigned256( 0x54c98c81 ),
      extraData       = ByteSeqMax1024( Array( 0x42.toByte ) ),
      mixHash         = EthHash.withBytes( "f6e588ee9247e2938e666ad73ca9830a32884468fae713819f60be52fa5f804d".decodeHex ),
      nonce           = Unsigned64( BigInt(1, "0xa2e63eb1bc75c82e".decodeHex ) )
    )
    EthBlock( header, Seq.empty, Seq.empty )
  }

  object Header {
    // from yellowpaper 4.3.4
    val GenesisDifficulty  = Unsigned256( 131072 ); 
    val LateChildThreshold = 8; // seconds

    def isValidChildOfParent( putativeChild : Header, putativeParent : Header ) : Boolean = {
      def numberMatches      = putativeChild.number.widen == putativeParent.number.widen + 1;
      def validTimestamp     = putativeChild.timestamp.widen > putativeParent.timestamp.widen;
      def legalGasLimit      = _legalGasLimit( putativeChild.gasLimit.widen, putativeParent.gasLimit.widen );
      def difficultyMatches  = childDifficulty( putativeChild.timestamp, putativeParent ) == putativeParent.difficulty;
      def hashMatches        = EthHash.hash( RLP.encode( putativeParent ) ) == putativeChild.parentHash;
      def proofOfWorkMatches = validateProofOfWork( putativeChild );

      // we check easiest first, to avoid hitting the hard stuff if we can
      numberMatches && validTimestamp && legalGasLimit && difficultyMatches && hashMatches && proofOfWorkMatches
    }

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

    private val _GenesisDifficulty = GenesisDifficulty.widen;

    object ProofOfWork {
      private[Header] val ThresholdNumerator : BigInt = BigInt(1) << 256;
    }
    case class ProofOfWork( m : EthHash, n : Unsigned256 );

    def proofOfWork( putativeNonce : Unsigned64, ignoreMixNonceHeader : Header ) : ProofOfWork = ???

    def validateProofOfWork( finalized : Header ) : Boolean = {
      val pow = proofOfWork( finalized.nonce, finalized );
      pow.m == finalized.mixHash && pow.n.widen <= (ProofOfWork.ThresholdNumerator / pow.n.widen)
    }
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
    mixHash         : EthHash         = AllZerosEthHash,
    nonce           : Unsigned64      = Unsigned64(0)
  ) {
    def isValidChild( putativeParent : Header ) : Boolean = Header.isValidChildOfParent( this, putativeParent );
  }
}
case class EthBlock( header : EthBlock.Header, transactions : Seq[EthTransaction], ommers : Seq[EthBlock.Header] );
