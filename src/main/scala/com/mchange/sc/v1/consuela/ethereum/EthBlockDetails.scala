/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP, RLPSerializable}
import specification.Types.{Unsigned64,Unsigned256,Unsigned2048,ByteSeqMax1024};

import pow.ProofOfWork;

import scala.collection.immutable.Seq;

object EthBlockDetails {
  // from yellowpaper 4.3.4
  val GenesisDifficulty  = Unsigned256( 1 << 17 ); // 1 << 17 == BigInt(2).pow(17) == 0x020000 == 131072
  val GenesisBlock = {
    import com.mchange.sc.v1.consuela._;

    // header state taken from ethereum/tests/BlockTests/bcTotalDifficultyTest.json 2015-05-05
    val header = EthBlock.Header(
      parentHash      = AllZeroesEthHash,
      ommersHash      = EthHash.withBytes("0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347".decodeHex),
      coinbase        = EthAddress( "0x8888f1f195afa192cfee860698584c030f4c9db1".decodeHex ),
      stateRoot       = EthHash.withBytes( "0x7dba07d6b448a186e9612e5f737d1c909dce473e53199901a302c00646d523c1".decodeHex ),
      transactionRoot = EthHash.withBytes( "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421".decodeHex ),
      receiptsRoot    = EthHash.withBytes( "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421".decodeHex ),
      logsBloom       = EthLogBloom.empty,
      difficulty      = GenesisDifficulty,
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

  final object Header {
    val LateChildThreshold = 8; // seconds

    def isValidChildOfParent( putativeChild : EthBlock.Header, putativeParent : EthBlock.Header ) : Boolean = {
      def numberMatches      = putativeChild.number.widen == putativeParent.number.widen + 1;
      def validTimestamp     = putativeChild.timestamp.widen > putativeParent.timestamp.widen;
      def legalGasLimit      = _legalGasLimit( putativeChild.gasLimit.widen, putativeParent.gasLimit.widen );
      def difficultyMatches  = childDifficulty( putativeChild.timestamp, putativeParent ) == putativeParent.difficulty;
      def hashMatches        = EthHash.hash( RLP.encode( putativeParent ) ) == putativeChild.parentHash;
      def proofOfWorkMatches = ProofOfWork.validate( putativeChild );

      // we check easiest first, to avoid hitting the hard stuff if we can
      numberMatches && validTimestamp && legalGasLimit && difficultyMatches && hashMatches && proofOfWorkMatches
    }

    def childDifficulty( childTimestamp : Unsigned256, parent : EthBlock.Header ) : Unsigned256 = {
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

    def childGasLimitRange( parent : EthBlock.Header ) : ( Unsigned256, Unsigned256 ) = {
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
  }
}
