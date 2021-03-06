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

import com.mchange.sc.v1.consuela.ethereum.{EthBlockDetails => Details}

import scala.collection._

object EthBlock {
  val Genesis : EthBlock = Details.GenesisBlock;

  final object Header {
    val NumFields = 15; // is there some more dynamic way to set this, that would track changes to the case class?
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
final case class EthBlock( header : EthBlock.Header, transactions : immutable.Seq[EthTransaction], ommers : immutable.Seq[EthBlock.Header] );
