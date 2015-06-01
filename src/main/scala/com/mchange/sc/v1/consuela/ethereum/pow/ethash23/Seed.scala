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

package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.hash.SHA3_256;

object Seed {
  private implicit lazy val logger = MLogger( this );

  case class Primer( epochNumber : Long, value : SHA3_256 );

  private val seedCache = new java.util.concurrent.ConcurrentSkipListMap[Long,SHA3_256];

  // we are doing some nonatomic stuff, but the worst a race will do is cause idempotent work to be duplicated
  def ensureCacheThruEpoch( thruEpochNumber : Long )( implicit  primer : Primer ) : Unit = {
    require( thruEpochNumber >= primer.epochNumber, s"To compute a seed hash, thruEpochNumber (${thruEpochNumber}) must be no less than primer.epochNumber ${primer.epochNumber}" );

    val lastPrecomputedEntry = {
      val tmp = seedCache.lastEntry();
      if ( tmp == null ) {
        seedCache.put( primer.epochNumber, primer.value );
        seedCache.lastEntry()
      } else {
        tmp
      }
    }
    val lastPrecomputedKey = lastPrecomputedEntry.getKey();

    var lastHash = lastPrecomputedEntry.getValue();
    var lastKey  = lastPrecomputedKey;
    while ( lastKey < thruEpochNumber ) {
      var nextKey = lastKey + 1;
      var nextHash = SHA3_256.hash( lastHash.bytes );
      seedCache.put( nextKey, nextHash );
      lastHash = nextHash;
      lastKey = nextKey;
    }

    if ( lastKey != lastPrecomputedKey ) {
      INFO.log( s"Seed cache populated from epoch number ${seedCache.firstKey} through epoch number ${lastKey}, whose seed is 0x${lastHash.bytes.hex}" )
    }
  }

  def getForEpoch( epochNumber : Long )( implicit primer : Primer ) : Array[Byte] = {
    ensureCacheThruEpoch( epochNumber )( primer );
    seedCache.get( epochNumber ).toByteArray
  }

  def ensureCacheThruBlock( blockNumber : Long ) : Unit = ensureCacheThruEpoch( epochFromBlock( blockNumber ) )

  def getForBlock( blockNumber : Long ) : Array[Byte] = getForEpoch( epochFromBlock( blockNumber ) )
}

