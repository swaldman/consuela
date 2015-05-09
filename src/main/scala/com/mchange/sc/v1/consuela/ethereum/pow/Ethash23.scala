package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela.hash.SHA3_512;

import scala.annotation.tailrec;

import spire.math.SafeLong;
import spire.implicits._

// is Long math good enough (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...

object Ethash23 {
  // implementing REVISION 23 of Ethash spec, defined https://github.com/ethereum/wiki/wiki/Ethash

  private final val WordBytes          = 1L << 2;  // 4
  private final val DatasetBytesInit   = 1L << 30; // 2 ** 30
  private final val DatasetBytesGrowth = 1L << 23; // 2 ** 23
  private final val CacheBytesInit     = 1L << 24; // 2 ** 24
  private final val CacheBytesGrowth   = 1L << 17; // 2 ** 17
  private final val CacheMultiplier    = 1L << 10; // 1024
  private final val EpochLength        = 30000L;   // 30000
  private final val MixBytes           = 1L << 7;  // 128
  private final val HashBytes          = 1L << 6;  // 64
  private final val DatasetParents     = 1L << 8;  // 256
  private final val CacheRounds        = 3L;       // 3
  private final val Accesses           = 1L << 6;  // 64

  private final val DoubleHashBytes = 2 * HashBytes;
  private final val DoubleMixBytes  = 2 * HashBytes;

  private final val FnvPrime = 0x01000193L;

  private final val ProbablePrimeCertainty : Int = 8; //arbitrary, tune for performance...

  private def getCacheSize( blockNumber : Long ) : Long = {
    @tailrec 
    def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / HashBytes ) ) sz else descendToPrime( sz - DoubleHashBytes );

    val start = CacheBytesInit + ( CacheBytesGrowth * ( blockNumber / EpochLength ) ) - HashBytes;
    descendToPrime( start )
  }

  private def getFullSize( blockNumber : Long ) : SafeLong = {
    @tailrec 
    def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / MixBytes ) ) sz else descendToPrime( sz - DoubleMixBytes );

    val start = DatasetBytesInit + ( DatasetBytesGrowth * ( blockNumber / EpochLength ) ) - MixBytes;
    descendToPrime( start )
  }

  // this seems very arcane...
  private def mkCache( cacheSize : Long, seed : Array[Byte] ) : Array[Array[Byte]] = {
    val n = cacheSize / HashBytes;

    require( n.isValidInt, s"n, our array size, must be a valid Int. but it's not, n = ${n}" ); 

    val nInt = n.toInt;
    val o = Array.iterate( SHA3_512.rawHash( seed ), nInt )( lastBytes => SHA3_512.rawHash( lastBytes ) )
    for (_ <- 0L until CacheRounds; i <- 0 until nInt ) {
      val v = o(i)(0) % nInt;
      o(i) = SHA3_512.rawHash( (o((i-1+nInt) % nInt), o(v)).zipped.map( (l, r) => (l ^ r).toByte ) )
    }
    o
  }

  private def fnv( v1 : Long, v2 : Long ) : SafeLong = ((v1 * FnvPrime) ^ v2) % (1 << 32)

  // we probably want to optimize this someday
  private def isPrime( num : SafeLong ) : Boolean = {
    def naiveIsPrime : Boolean = {
      val limit = num.sqrt;
      var check = 2;
      while (check <= limit) if( num % check == 0 ) return false else check += 1;
      return true;
    }
    def probablePrime = num.isProbablePrime( ProbablePrimeCertainty )

    probablePrime && naiveIsPrime
  }
}
