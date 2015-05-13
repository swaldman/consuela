package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.encoding._;

import ethereum.specification.Types.Unsigned64;

import com.mchange.sc.v1.consuela.hash.{SHA3_256,SHA3_512};

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import com.mchange.lang.IntegerUtils;

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

  implicit final class PlusModInt( val i : Int ) extends AnyVal {
    def +%( other : Int ) = scala.math.abs( i % other )
  }
  implicit final class PlusModLong( val l : Long ) extends AnyVal {
    def +%( other : Int ) : Long  = scala.math.abs( l % other )
    def +%( other : Long ) : Long = scala.math.abs( l % other )
  }

  def epochFromBlock( blockNumber : Long ) : Long = ( blockNumber / EpochLength )
  def blocksRemainingInEpoch( blockNumber : Long ) : Long = EpochLength - ( blockNumber % EpochLength )

  final class Hashimoto( val mixDigest : Array[Byte], val result : Array[Byte] ) {
    override def toString : String = s"Hashimote(mixDigest=${mixDigest.hex},result=${result.hex})"
  }

  object Manager {
    val Default = UInt32AsLong;

    object UInt32AsLong extends Manager {
      type Cache   = Array[Array[Long]]
      type Dataset = Array[Array[Long]]

      def mkCacheForBlock( blockNumber : Long ) : Cache = mkCacheForEpoch( epochFromBlock( blockNumber ) );

      def mkCacheForEpoch( epochNumber : Long ) : Cache = {
        val cacheSize = getCacheSizeForEpoch( epochNumber );
        val seed      = Seed.getForEpoch( epochNumber );
        mkCache( cacheSize, seed );
      }

      def calcDatasetForBlock( blockNumber : Long ) : Dataset = calcDatasetForEpoch( epochFromBlock( blockNumber ) );

      def calcDatasetForEpoch( epochNumber : Long ) : Dataset = {
        val cache = mkCacheForEpoch( epochNumber );
        val fullSize = getFullSizeForEpoch( epochNumber );
        calcDataset( cache, fullSize )
      }

      def hashimotoLight( header : EthBlock.Header, cache : Cache, nonce : Unsigned64 ) : Hashimoto = {
        val blockNumber = assertValidLong( header.number.widen );
        hashimotoLight( getFullSizeForBlock( blockNumber ), cache, truncatedHeaderHash( header ), nonce )
      }

      def hashimotoFull( header : EthBlock.Header, dataset : Dataset, nonce : Unsigned64 ) : Hashimoto = {
        val blockNumber = assertValidLong( header.number.widen );
        hashimotoFull( getFullSizeForBlock( blockNumber ), dataset, truncatedHeaderHash( header ), nonce )
      }

      // rawBytes.length must be divisble by four, or ArrayIndexOutOfBoundsException
      private def asUnsignedLittleEndianInts( rawBytes : Array[Byte] ) : Array[Long] = {
        Array.range(0, rawBytes.length, 4).map( IntegerUtils.intFromByteArrayLittleEndian( rawBytes, _ ) & 0xFFFFFFFFL )
      }

      // rawBytes.length must be divisble by four, or ArrayIndexOutOfBoundsException
      private def asLittleEndianIntBytes( rawBytes : Array[Byte] ) : Array[Byte] = {
        val len = rawBytes.length;
        val out = Array.ofDim[Byte]( rawBytes.length );
        def mapIndex( srcIndex : Int ) : Int = {
          val base = srcIndex / 4;
          val offset = 3 - (srcIndex % 4);
          base + offset
        }
        var i = 0;
        while ( i < len ) {
          out(i) = rawBytes( mapIndex(i) )
          i += 1;
        }
        out
      }

      private def crunchFlattenNoSwap( in : Array[Long] ) : Array[Byte] = {
        val inLen = in.length;
        val out = Array.ofDim[Byte]( inLen * 4 );
        var i = 0;
        while ( i < inLen ) {
          IntegerUtils.intIntoByteArray( in(i).toInt, i * 4, out );
          i += 1
        }
        out
      }

      private def crunchFlattenSwap( in : Array[Long] ) : Array[Byte] = {
        val inLen = in.length;
        val out = Array.ofDim[Byte]( inLen * 4 );
        var i = 0;
        while ( i < inLen ) {
          IntegerUtils.intIntoByteArrayLittleEndian( in(i).toInt, i * 4, out );
          i += 1
        }
        out
      }

      private def sha3_512_readUnsignedLittleEndianInts( stuff : Array[Byte] ) : Array[Long] = asUnsignedLittleEndianInts( SHA3_512.rawHash( stuff ) )

      private def assertValidInt( l : Long )     : Int  = if (l.isValidInt) l.toInt else throw new AssertionError( s"${l} is not a valid Int, as required." );
      private def assertValidLong( bi : BigInt ) : Long = if (bi.isValidLong) bi.toLong else throw new AssertionError( s"${bi} is not a valid Long, as required." );

      // this seems very arcane...
      private def mkCache( cacheSize : Long, seed : Array[Byte] ) : Array[Array[Long]] = {
        val n = assertValidInt( cacheSize / HashBytes );
        val o = Array.iterate( sha3_512_readUnsignedLittleEndianInts( seed ), n )( lastLongs => sha3_512_readUnsignedLittleEndianInts( crunchFlattenSwap( lastLongs ) ) )
        for (_ <- 0L until CacheRounds; i <- 0 until n ) {
          val v = (o(i)(0) +% n).toInt;
          o(i) = sha3_512_readUnsignedLittleEndianInts( crunchFlattenSwap( (o((i-1+n) % n), o(v)).zipped.map( (l, r) => (l ^ r) ) ) )
        }
        o
      }

      def hashCache( cache : Cache ) : SHA3_256 = SHA3_256.hash( cache.flatMap( crunchFlattenSwap ) )

      //private def fourLittleEndianBytesAsUnsigned( src : Array[Byte], wordOffset : Int ) = IntegerUtils.toUnsigned( IntegerUtils.intFromByteArrayLittleEndian( src, wordOffset * 4 ) );

      private def calcDatasetItem( cache : Cache, i : Int ) : Array[Long] = {
        val n = cache.length;
        val r = HashBytesOverWordBytes;

        val mix = {
          val tmp = cache( i +% n ).clone;
          tmp(0) = tmp(0) ^ i;
          sha3_512_readUnsignedLittleEndianInts( crunchFlattenSwap( tmp ) )
        }

        @tailrec
        def remix( lastMix : Array[Long], count : Int = 0) : Array[Long] = {
          count match {
            case DatasetParents => lastMix;
            case j              => {
              val cacheIndex = fnv( i ^ j, lastMix( j % r ) );
              val nextMix = ( lastMix, cache( (cacheIndex +% n).toInt ) ).zipped.map( (a, b) => fnv( a, b ) )
              remix( nextMix, count + 1 )
            }
          }
        }

        sha3_512_readUnsignedLittleEndianInts( crunchFlattenSwap( remix( mix ) ) )
      }

      private def calcDataset( cache : Cache, fullSize : Long ) : Array[Array[Long]] = {
        val len = assertValidInt( fullSize / HashBytes );
        val out = Array.ofDim[Array[Long]]( len );
        (0 until len).foreach( i => out(i) = calcDatasetItem( cache, i ) );
        out
      }

      private def combineIndexedArrays( srcAccessor : Int => Array[Long], srcLen : Int, times : Int ) : Array[Long] = {
        val arraylen = assertValidInt( srcLen.toLong * times.toLong );
        val tmp = Array.ofDim[Long]( arraylen );
        (0 until times).foreach( i => System.arraycopy( srcAccessor(i), 0, tmp, i * srcLen, srcLen ) )
        tmp
      }

      private def replicateArray( src : Array[Long], srcLen : Int, times : Int ) : Array[Long] = combineIndexedArrays( _ => src, srcLen, times );

      private def hashimoto(truncatedHeaderHash : SHA3_256, nonce : Unsigned64 , fullSize : Long, datasetAccessor : Int => Array[Long] ) : Hashimoto = {
        hashimoto( ( truncatedHeaderHash.bytes ++ nonce.widen.unsignedBytes(8).reverse ).toArray, fullSize, datasetAccessor )
      }

      private def hashimoto( seedBytes : Array[Byte], fullSize : Long, datasetAccessor : Int => Array[Long] ) : Hashimoto = {
        val n = fullSize / HashBytes;
        val w = MixBytesOverWordBytes;
        val mixHashes = MixBytesOverHashBytes;

        val s = sha3_512_readUnsignedLittleEndianInts( seedBytes );
        val len = s.length;

        val startMix : Array[Long] = replicateArray(s, len, mixHashes);

        @tailrec
        def remix( lastMix : Array[Long], i : Int = 0 ) : Array[Long] = {
          i match {
            case Accesses => lastMix;
            case _        => {
              val p = ( fnv( i ^ s(0), lastMix( i % w ) ) % (n / mixHashes) ) * mixHashes;
              //val p = (( fnv( i ^ fourLittleEndianBytesAsUnsigned(s, 0), fourLittleEndianBytesAsUnsigned( lastMix, (i % w) ) ) % (n / mixHashes) ) * mixHashes).toInt;
              val offsetAccessor : Int => Array[Long] = j => datasetAccessor( assertValidInt( p + j ) );

              // note that we are looking up fixed-length hashes
              // all the same length as our seed hash, so len is fine
              val newData = combineIndexedArrays( offsetAccessor, len, mixHashes );
              val nextMix = ( lastMix, newData ).zipped.map( (a, b) => fnv( a, b ) )
              remix( nextMix, i + 1 )
            }
          }
        }

        val uncompressedMix = remix( startMix );

        def compressNextFour( src : Array[Long], offset : Int ) : Long = {
          def srcval( i : Int ) = src( offset + i );
          fnv( fnv( fnv( srcval(0), srcval(1) ), srcval(2) ), srcval(3) )
        }

        val compressedMix = Array.range(0, uncompressedMix.length, 4).map( i => compressNextFour( uncompressedMix, i ) );

        val mixDigest = crunchFlattenSwap( compressedMix );
        val result = SHA3_256.rawHash( crunchFlattenSwap( s ++ compressedMix ) )

        new Hashimoto( mixDigest, result )
      }

      private def hashimotoLight( fullSize : Long, cache : Cache, truncatedHeaderHash : SHA3_256, nonce : Unsigned64 ) : Hashimoto = {
        hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => calcDatasetItem( cache, i ) )
      }

      private def hashimotoFull( fullSize : Long, dataset : Dataset, truncatedHeaderHash : SHA3_256, nonce : Unsigned64 ) : Hashimoto = {
        hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => dataset( i ) )
      }

      private def fnv( v1 : Long, v2 : Long ) : Long = ((v1 * FnvPrime) ^ v2) % (1L << 32)
    }
  }
  trait Manager {

    //abstract members
    type Cache;
    type Dataset;

    def mkCacheForBlock( blockNumber : Long ) : Cache;
    def mkCacheForEpoch( blockNumber : Long ) : Cache;

    def calcDatasetForBlock( blockNumber : Long ) : Dataset;
    def calcDatasetForEpoch( blockNumber : Long ) : Dataset;

    def hashimotoLight( header : EthBlock.Header, cache : Array[Array[Long]], nonce : Unsigned64 ) : Hashimoto;
    def hashimotoFull( header : EthBlock.Header, dataset : Array[Array[Long]], nonce : Unsigned64 ) : Hashimoto;

    // public utilities
    def epochFromBlock( blockNumber : Long )         : Long = Ethash23.epochFromBlock( blockNumber );
    def blocksRemainingInEpoch( blockNumber : Long ) : Long = Ethash23.blocksRemainingInEpoch( blockNumber );

    // protected utilities
    protected[pow] def getCacheSizeForBlock( blockNumber : Long ) : Long = getCacheSizeForEpoch( epochFromBlock( blockNumber ) );

    protected[pow] def getCacheSizeForEpoch( epochNumber : Long ) : Long = {
      @tailrec
      def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / HashBytes ) ) sz else descendToPrime( sz - DoubleHashBytes );

      val start = CacheBytesInit + ( CacheBytesGrowth * epochNumber ) - HashBytes;
      descendToPrime( start )
    }

    protected[pow] def getFullSizeForBlock( blockNumber : Long ) : Long = getFullSizeForEpoch( epochFromBlock( blockNumber ) );

    protected[pow] def getFullSizeForEpoch( epochNumber : Long ) : Long = {
      @tailrec
      def descendToPrime( sz : Long ) : Long = if ( isPrime( sz / MixBytes ) ) sz else descendToPrime( sz - DoubleMixBytes );

      val start = DatasetBytesInit + ( DatasetBytesGrowth * epochNumber ) - MixBytes;
      descendToPrime( start )
    }

    /*
     * omit the last two elements, 
     * convert truncated header to RLP, 
     * take SHA3_256 hash
     */ 
    protected[pow] def truncatedHeaderHash( header : EthBlock.Header ) : SHA3_256 = {
      val headerElement = RLP.toElement[EthBlock.Header]( header );
      val RLP.Element.Seq( fullSeq ) = headerElement;
      val numToKeep = fullSeq.length - 2;
      val truncSeq = fullSeq.take( numToKeep );
      val truncHeaderRLP = RLP.Element.encode( RLP.Element.Seq( truncSeq ) )
      SHA3_256.hash( truncHeaderRLP )
    }

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
