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

import com.mchange.sc.v1.consuela.bloom.Bloom;

import scala.collection._;

import com.mchange.lang.LongUtils.longFromByteArray;

object EthBloom {
  def indicesFromEthHash( hash : EthHash ) : immutable.Set[Int] = {
    val bottom8 : Long = longFromByteArray( hash.toByteArray, EthHash.HashLength - 8 );
    val mask    : Long = 0x07FF; // the eleven least-significant-bits set

    def uint11( offset : Int ) : Int = ((bottom8 & (mask << offset) >>> offset)).toInt
    immutable.Set( uint11(0), uint11(16), uint11(32) )
  }

  abstract class Definition[T]( xform : T => EthHash ) extends Bloom.Definition[T] {
    def indices( t : T ) : immutable.Set[Int] = indicesFromEthHash( xform(t) ); // no need to mod, since these are 11 bit values

    val NumHashes : Int = 3
    val BitLength : Int = 1 << 11; // 1 << 11 == 2048
  }
}
