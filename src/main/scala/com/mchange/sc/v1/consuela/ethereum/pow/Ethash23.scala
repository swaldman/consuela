package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela._;

import ethereum.specification.Types.Unsigned64;

import com.mchange.sc.v1.consuela.hash.{SHA3_256,SHA3_512};

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import scala.annotation.tailrec;

//import spire.math.SafeLong;
import spire.implicits._

// is Long math good enough? (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...

object Ethash23 {
  // implementing REVISION 23 of Ethash spec, defined https://github.com/ethereum/wiki/wiki/Ethash

  private final val WordBytes          = 1 << 2;  // 4
  private final val DatasetBytesInit   = 1 << 30; // 2 ** 30
  private final val DatasetBytesGrowth = 1 << 23; // 2 ** 23
  private final val CacheBytesInit     = 1 << 24; // 2 ** 24
  private final val CacheBytesGrowth   = 1 << 17; // 2 ** 17
  private final val CacheMultiplier    = 1 << 10; // 1024
  private final val EpochLength        = 30000;   // 30000
  private final val MixBytes           = 1 << 7;  // 128
  private final val HashBytes          = 1 << 6;  // 64
  private final val DatasetParents     = 1 << 8;  // 256
  private final val CacheRounds        = 3;       // 3
  private final val Accesses           = 1 << 6;  // 64

  private final val DoubleHashBytes = 2 * HashBytes; //128
  private final val DoubleMixBytes  = 2 * HashBytes; //256

  private final val HashBytesOverWordBytes = HashBytes / WordBytes; //16
  private final val MixBytesOverWordBytes  = MixBytes / WordBytes;  //32
  private final val MixBytesOverHashBytes  = MixBytes / HashBytes;  //2

  private final val FnvPrime = 0x01000193;

  private final val ProbablePrimeCertainty : Int = 8; //arbitrary, tune for performance...

  def epochFromBlock( blockNumber : Long ) : Long = ( blockNumber / EpochLength )

  def blocksRemainingInEpoch( blockNumber : Long ) : Long = EpochLength - ( blockNumber % EpochLength )

  def getCacheSizeForBlock( blockNumber : Long ) : Long = getCacheSizeForEpoch( epochFromBlock( blockNumber ) );

  def getCacheSizeForEpoch( epochNumber : Long ) : Long = {
    @tailrec 
    def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / HashBytes ) ) sz else descendToPrime( sz - DoubleHashBytes );

    val start = CacheBytesInit + ( CacheBytesGrowth * epochNumber ) - HashBytes;
    descendToPrime( start )
  }

  def getFullSizeForBlock( blockNumber : Long ) : Long = getFullSizeForEpoch( epochFromBlock( blockNumber ) );

  def getFullSizeForEpoch( epochNumber : Long ) : Long = {
    @tailrec 
    def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / MixBytes ) ) sz else descendToPrime( sz - DoubleMixBytes );

    val start = DatasetBytesInit + ( DatasetBytesGrowth * epochNumber ) - MixBytes;
    descendToPrime( start )
  }

  def mkCacheForBlock( blockNumber : Long ) : Array[Array[Byte]] = mkCacheForEpoch( epochFromBlock( blockNumber ) );

  def mkCacheForEpoch( epochNumber : Long ) : Array[Array[Byte]] = {
    val cacheSize = getCacheSizeForEpoch( epochNumber );
    val seed      = Seed.getForEpoch( epochNumber );
    mkCache( cacheSize, seed );
  }

  // this seems very arcane...
  private def mkCache( cacheSize : Long, seed : Array[Byte] ) : Array[Array[Byte]] = {
    val n = {
      val tmp = cacheSize / HashBytes;
      require( tmp.isValidInt, s"n, our array size, must be a valid Int. but it's not, n = ${tmp}" );
      tmp.toInt
    }
    val o = Array.iterate( SHA3_512.rawHash( seed ), n )( lastBytes => SHA3_512.rawHash( lastBytes ) )
    for (_ <- 0L until CacheRounds; i <- 0 until n ) {
      val v = o(i)(0) % n;
      o(i) = SHA3_512.rawHash( (o((i-1+n) % n), o(v)).zipped.map( (l, r) => (l ^ r).toByte ) )
    }
    o
  }

  def calcDatasetForBlock( blockNumber : Long ) : Array[Array[Byte]] = calcDatasetForEpoch( epochFromBlock( blockNumber ) );

  def calcDatasetForEpoch( epochNumber : Long ) : Array[Array[Byte]] = {
    val cache = mkCacheForEpoch( epochNumber );
    val fullSize = getFullSizeForEpoch( epochNumber );
    calcDataset( cache, fullSize )
  }

  // note the lots of narrowing of integral types here. 
  // i think it's all right, but let's see.
  private def calcDatasetItem( cache : Array[Array[Byte]], i : Int ) : Array[Byte] = {
    val n = cache.length;
    val r = HashBytesOverWordBytes;

    val mix = {
      val tmp = cache(i % n).clone;
      tmp(0) = (tmp(0) ^ i).toByte;
      SHA3_512.rawHash( tmp )
    }

    @tailrec
    def remix( lastMix : Array[Byte], count : Int = 0) : Array[Byte] = {
      count match {
        case DatasetParents => lastMix;
        case j              => {
          val cacheIndex = fnv( i ^ j, lastMix( j % r ) );
          val nextMix = ( lastMix, cache( (cacheIndex % n).toInt ) ).zipped.map( (a, b) => fnv( a, b ).toByte )
          remix( nextMix, count + 1 )
        }
      }
    }

    SHA3_512.rawHash( remix( mix ) )
  }

  private def calcDataset( cache : Array[Array[Byte]], fullSize : Long ) : Array[Array[Byte]] = {
    val len = {
      val tmp = fullSize / HashBytes;
      require( tmp.isValidInt, s"len, our dataset length, must be a valid Int. but it's not, len = ${tmp}" );
      tmp.toInt;
    }
    val out = Array.ofDim[Array[Byte]]( len );
    (0 until len).foreach( i => out(i) = calcDatasetItem( cache, i ) );
    out
  }

  private def combineIndexedArrays( srcAccessor : Int => Array[Byte], srcLen : Int, times : Int ) : Array[Byte] = {
    val arraylen = {
      val longlen : Long = srcLen.toLong * times.toLong;
      require( longlen.isValidInt, s"arraylen, our combine array length, must be a valid Int. but it's not, arraylen = ${longlen}" );
      longlen.toInt
    }
    val tmp = Array.ofDim[Byte]( arraylen );
    (0 until times).foreach( i => System.arraycopy( srcAccessor(i), 0, tmp, i * srcLen, srcLen ) )
    tmp
  }

  private def replicateArray( src : Array[Byte], srcLen : Int, times : Int ) : Array[Byte] = combineIndexedArrays( _ => src, srcLen, times );

  final class Hashimoto( mixDigest : Array[Byte], result : Array[Byte] );

  private def hashimoto(truncatedHeaderRLP : Seq[Byte], nonce : Unsigned64 , fullSize : Int, datasetAccessor : Int => Array[Byte] ) : Hashimoto = {
    hashimoto( ( truncatedHeaderRLP ++ nonce.widen.unsignedBytes(8) ).toArray, fullSize, datasetAccessor )
  }

  private def hashimoto( seedBytes : Array[Byte], fullSize : Int, datasetAccessor : Int => Array[Byte] ) : Hashimoto = {
    val n = fullSize / HashBytes;
    val w = MixBytesOverWordBytes;
    val mixHashes = MixBytesOverHashBytes;

    val s = SHA3_512.rawHash( seedBytes );
    val len = s.length;

    val startMix : Array[Byte] = replicateArray(s, len, mixHashes);

    @tailrec
    def remix( lastMix : Array[Byte], i : Int = 0 ) : Array[Byte] = {
      i match {
        case Accesses => lastMix;
        case _        => {
          val p = (( fnv( i ^ s(0), lastMix( i % w ) ) % (n / mixHashes) ) * mixHashes).toInt;
          val offsetAccessor : Int => Array[Byte] = j => datasetAccessor( p + j );

          // note that we are looking up fixed-length hashes 
          // all the same length as our seed hash, so len is fine
          val newData = combineIndexedArrays( offsetAccessor, len, mixHashes );
          val nextMix = ( lastMix, newData ).zipped.map( (a, b) => fnv( a, b ).toByte )
          remix( nextMix, i + 1 )
        }
      }
    }

    val uncompressedMix = remix( startMix );

    def compressNextFour( src : Array[Byte], offset : Int ) : Byte = {
      def srcval( i : Int ) = src( offset + i );
      fnv( fnv( fnv( srcval(0), srcval(1) ), srcval(2) ), srcval(3) ).toByte
    }

    val compressedMix = Array.range(0, uncompressedMix.length, 4).map( i => compressNextFour( uncompressedMix, i ) );

    val mixDigest = compressedMix;
    val result = SHA3_256.rawHash( s ++ compressedMix )

    new Hashimoto( mixDigest, result )
  }

  private def fnv( v1 : Long, v2 : Long ) : Long = ((v1 * FnvPrime) ^ v2) % (1 << 32)

  // we probably want to optimize this someday
  private def isPrime( num : Long ) : Boolean = {
    def naiveIsPrime : Boolean = {
      val limit = num.sqrt;
      var check = 2;
      while (check <= limit) if( num % check == 0 ) return false else check += 1;
      return true;
    }
    def probablePrime = BigInt(num).isProbablePrime( ProbablePrimeCertainty )

    probablePrime && naiveIsPrime
  }

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

      INFO.log( s"Seed cache populated from epoch number ${seedCache.firstKey} through epoch number ${lastKey}, whose seed is 0x${lastHash.bytes.hex}" )
    }

    def getForEpoch( epochNumber : Long )( implicit primer : Primer ) : Array[Byte] = {
      ensureCacheThruEpoch( epochNumber )( primer );
      seedCache.get( epochNumber ).toByteArray
    }

    def ensureCacheThruBlock( blockNumber : Long ) : Unit = ensureCacheThruEpoch( epochFromBlock( blockNumber ) )

    def getForBlock( blockNumber : Long ) : Array[Byte] = getForEpoch( epochFromBlock( blockNumber ) )
  }
}
