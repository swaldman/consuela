package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.hash.SHA3_256;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

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

