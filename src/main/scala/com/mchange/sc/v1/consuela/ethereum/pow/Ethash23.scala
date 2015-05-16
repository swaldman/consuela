package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum._;
import com.mchange.sc.v1.consuela.ethereum.encoding._;

import ethereum.specification.Types.{Unsigned64,Unsigned256};

import com.mchange.sc.v1.consuela.hash.{SHA3_256,SHA3_512};

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import com.mchange.lang.IntegerUtils;

import scala.collection._;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import scala.annotation.tailrec;

//import spire.math.SafeLong;
import spire.implicits._

import scala.reflect.ClassTag;

// is Long math good enough? (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...
// but when the dataset gets bigger than ~8GB, we'll exceed int array indices, and have to encode
// values into fully packed longs, rather than ints or longs representing unsigned ints as currently
// implements. (the current implementation will fail fast when this limit is exceeded.)

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

  private final val RowWidth = 16; // four-byte Ints

  implicit final class PlusModInt( val i : Int ) extends AnyVal {
    def +%( other : Int ) = scala.math.abs( i % other )
  }
  implicit final class PlusModLong( val l : Long ) extends AnyVal {
    def +%( other : Int ) : Long  = scala.math.abs( l % other )
    def +%( other : Long ) : Long = scala.math.abs( l % other )
  }

  def epochFromBlock( blockNumber : Long ) : Long = ( blockNumber / EpochLength )
  def blocksRemainingInEpoch( blockNumber : Long ) : Long = EpochLength - ( blockNumber % EpochLength )

  case class Hashimoto( val mixDigest : immutable.Seq[Byte], val result : Unsigned256 ) {
    override def toString : String = s"Hashimote(mixDigest=${mixDigest.hex},result=${result.widen.unsignedBytes(32).hex})"
  }

  /*
   *  OK, so this is some terrible code reuse, but I couldn't figure out a good way to write this performance-sensitive,
   *  primitive-based code generically without boxing and with decent syntax. I did try! see
   * 
   *      https://github.com/swaldman/consuela/blob/abortive-generic-ethash-manager/src/main/scala/com/mchange/sc/v1/consuela/ethereum/pow/Ethash23.scala 
   *      https://github.com/swaldman/consuela/commit/a4e69183abfeaef1aa5717fd715824c1438d2a5f
   * 
   *  for an abortive attempt.
   */ 
  object Manager {
    private implicit lazy val logger = MLogger( this );

    lazy val Default = ParallelUInt32AsInt;

    object SequentialUInt32AsInt extends UInt32AsInt;
    object ParallelUInt32AsInt extends UInt32AsInt with Parallel;
    object LoggingSequentialUInt32AsInt extends UInt32AsInt with Logging;
    object LoggingParallelUInt32AsInt extends UInt32AsInt with Parallel with Logging;

    // for reasons I don't quite understand, embedding these implicitlys in object initializers directly,
    // lazy or not, led to errors in initialization ( null pointer or stack overflow). by trial and error,
    // this lazy computation in the parent object resolves the problem
    lazy val IntArrayClassTag  = implicitly[ClassTag[Array[Int]]];
    lazy val LongArrayClassTag = implicitly[ClassTag[Array[Long]]];

    trait Parallel extends Manager {
      override protected[pow] def calcDataset( cache : Cache, fullSize : Long ) : Dataset = calcDatasetParallel( cache, fullSize )
    }

    trait Logging extends Manager { // must not be specified before Parallel, or else pre- and post- logging won't happen
      lazy val parModifier = if ( this.isInstanceOf[Parallel] ) "parallel" else "sequential";

      override def mkCacheForEpoch( epochNumber : Long ) : Cache = {
        val startBlock = epochNumber * EpochLength;
        val lastBlock  = startBlock + EpochLength - 1;
        val start = System.currentTimeMillis();
        INFO.log( s"Beginning computation of cache for epoch ${epochNumber} (blocks ${startBlock} thru ${lastBlock})" );
        val out = super.mkCacheForEpoch( epochNumber );
        val done = System.currentTimeMillis();
        val secs = ( done - start ) / 1000d
        INFO.log( s"Completed computation of cache for epoch ${epochNumber} (blocks ${startBlock} thru ${lastBlock}) in ${secs} seconds" );
        out
      }

      override protected[pow] def calcDataset( cache : Cache, fullSize : Long ) : Dataset = {
        val start = System.currentTimeMillis();
        INFO.log( s"Beginning ${parModifier} computation of dataset, fullSize=${fullSize}, rows=${datasetLen(fullSize)}" );
        val out = super.calcDataset( cache, fullSize );
        val done = System.currentTimeMillis();
        val secs = ( done - start ) / 1000d
        INFO.log( s"Completed ${parModifier} computation of dataset in ${secs} seconds, fullSize=${fullSize}, rows=${datasetLen(fullSize)}" );
        out
      }
      abstract override protected[pow] def calcDatasetRow( cache : Cache, i : Int ) : Row = {
        val out = super.calcDatasetRow( cache : Cache, i : Int );
        INFO.log( s"Computed dataset row #${i}" )
        out
      }
    }

    class UInt32AsInt extends Manager {
      type Cache       = Array[Array[Int]]
      type Dataset     = Array[Array[Int]]
      type Row         = Array[Int]

      protected implicit val rowClassTag : ClassTag[Row] = IntArrayClassTag;

      /*
       *  we must read rawBytes as 4-byte little-endian unsigned ints, but we pack them into signed ints
       * 
       *  rawBytes.length must be divisble by four, or ArrayIndexOutOfBoundsException
       */ 
      private def readRow( rawBytes : Array[Byte] ) : Row = Array.range(0, rawBytes.length, 4).map( IntegerUtils.intFromByteArrayLittleEndian( rawBytes, _ ) )

      /*
       *  we must write the unsigned ints packed into the row's Longs as 4-byte little-endian unsigned ints
       * 
       */ 
      private def writeRow( in : Row ) : Array[Byte] = {
        val inLen = in.length;
        val out = Array.ofDim[Byte]( inLen * 4 );
        var i = 0;
        while ( i < inLen ) {
          IntegerUtils.intIntoByteArrayLittleEndian( in(i), i * 4, out );
          i += 1
        }
        out
      }

      private def sha3_512_readRow( stuff : Array[Byte] ) : Row = readRow( SHA3_512.rawHash( stuff ) )

      private def sha3_512_roundTrip( start : Array[Int] ) : Row = readRow( SHA3_512.rawHash( writeRow( start ) ) )

      private def sha3_512_roundTrip_DESTRUCTIVE( start : Array[Int] ) : Row = {
        val asBytes = writeRow( start );
        val asHash = SHA3_512.rawHash( asBytes )

        var len = start.length;
        var i = 0;
        while( i < len ) {
          start(i) = IntegerUtils.intFromByteArrayLittleEndian( asHash, i * 4 );
          i += 1;
        }
        start
      }

      // this seems very arcane...
      protected def mkCache( cacheSize : Long, seed : Array[Byte] ) : Cache = {
        val n = requireValidInt( cacheSize / HashBytes );
        val o = Array.iterate( sha3_512_readRow( seed ), n )( last => sha3_512_readRow( writeRow( last ) ) )
        for (_ <- 0L until CacheRounds; i <- 0 until n ) {
          val v = (UP( o(i)(0) ) +% n).toInt;
          o(i) = sha3_512_roundTrip_DESTRUCTIVE( zipWithXor(o( (i-1+n) % n ), o(v)) )
        }
        o
      }

      def hashCache( cache : Cache ) : SHA3_256 = SHA3_256.hash( cache.flatMap( writeRow ) )

      // for memoization / caching to files
      def dumpDatasetBytes( os : java.io.OutputStream, dataset : Dataset ) : Unit = dataset.foreach( iarr => os.write( writeRow( iarr ) ) )

      // this is ugly, but since these files are gigantic, i'm avoiding abstractions that
      // might require preloading or carry much overhead per byte or row
      def readDatasetBytes( is : java.io.InputStream ) : Dataset = {
        val bufferLen = RowWidth * 4;
        val buffer = Array.ofDim[Byte]( bufferLen );

        def handleRow : Array[Int] = {
          var b = is.read();
          if ( b < 0 ) {
            null //so sue me
          } else {
            var i = 0;
            var done = false;
            while (!done) {
              buffer(i) = b.toByte;
              i += 1;
              if (i == bufferLen) {
                done = true;
              } else {
                b = is.read();
                if (b < 0) throw new java.io.EOFException("Unexpected EOF reading byte ${i} of Ethash23.Dataset row! (should be ${bufferLen} bytes)");
              }
            } 
            // ok, we've filled the buffer, now we just have to interpret
            readRow( buffer )
          }
        }

        val arrays = scala.collection.mutable.ArrayBuffer.empty[Array[Int]];
        var row = handleRow;
        while ( row != null ) arrays += row;
        arrays.toArray
      }

      // unsigned promote
      private def UP( i : Int ) : Long = i & 0xFFFFFFFFL

      protected[pow] def extractDatasetRow( dataset : Dataset, i : Int ) : Row = dataset(i)

      protected[pow] def calcDatasetRow( cache : Cache, i : Int ) : Row = {
        val n = cache.length;
        val r = HashBytesOverWordBytes;

        val mix = {
          val tmp = cache( i % n ).clone;
          tmp(0) = tmp(0) ^ i;
          sha3_512_roundTrip_DESTRUCTIVE( tmp )
        }

        @tailrec
        def remix( lastMix : Row, count : Int = 0) : Row = {
          count match {
            case DatasetParents => lastMix;
            case j              => {
              val cacheIndex = fnv( i ^ j, lastMix( j % r ) );
              val nextMix = zipWithFnv_DESTRUCTIVE( lastMix, cache( (UP(cacheIndex) +% n).toInt ) )
              remix( nextMix, count + 1 )
            }
          }
        }

        sha3_512_roundTrip_DESTRUCTIVE( remix( mix ) )
      }

      protected[pow] def toDataset( array : Array[Row] ) : Dataset = array;

      private def combineIndexedArrays( srcAccessor : Int => Row, srcLen : Int, times : Int ) : Row = {
        val arraylen = requireValidInt( srcLen.toLong * times.toLong );
        val tmp = Array.ofDim[Int]( arraylen );
        (0 until times).foreach( i => System.arraycopy( srcAccessor(i), 0, tmp, i * srcLen, srcLen ) )
        tmp
      }

      private def replicateArray( src : Row, srcLen : Int, times : Int ) : Row = combineIndexedArrays( _ => src, srcLen, times );

      protected def hashimoto( seedBytes : Array[Byte], fullSize : Long, datasetAccessor : Int => Row ) : Hashimoto = {
        val n = fullSize / HashBytes;
        val w = MixBytesOverWordBytes;
        val mixHashes = MixBytesOverHashBytes;

        val s = sha3_512_readRow( seedBytes );
        val len = s.length;

        val startMix : Row = replicateArray(s, len, mixHashes);

        @tailrec
        def remix( lastMix : Row, i : Int = 0 ) : Row = {
          i match {
            case Accesses => lastMix;
            case _        => {
              val p = ( UP( fnv( i ^ s(0), lastMix( i % w ) ) ) % (n / mixHashes) ) * mixHashes;
              val offsetAccessor : Int => Row = j => datasetAccessor( requireValidInt( p + j ) );

              // note that we are looking up fixed-length hashes
              // all the same length as our seed hash, so len is fine
              val newData = combineIndexedArrays( offsetAccessor, len, mixHashes );
              val nextMix = zipWithFnv_DESTRUCTIVE( lastMix, newData )
              remix( nextMix, i + 1 )
            }
          }
        }

        val uncompressedMix = remix( startMix );

        def compressNextFour( src : Row, offset : Int ) : Int = {
          def srcval( i : Int ) = src( offset + i );
          fnv( fnv( fnv( srcval(0), srcval(1) ), srcval(2) ), srcval(3) )
        }

        val compressedMix = Array.range(0, uncompressedMix.length, 4).map( i => compressNextFour( uncompressedMix, i ) );

        val mixDigest = writeRow( compressedMix );
        val result = SHA3_256.rawHash( writeRow( s ++ compressedMix ) )

        new Hashimoto( ImmutableArraySeq.Byte( mixDigest ), Unsigned256( BigInt( 1, result ) ) )
      }

      // using Tuple2.zipped.map causes serious memory stress, so this
      private def zipWithXor( ls : Array[Int], rs : Array[Int] ) : Array[Int] = {
        val len = ls.length;
        val out = Array.ofDim[Int]( len );
        var i = 0;
        while (i < len ) {
          out(i) = ls(i) ^ rs(i);
          i += 1
        }
        out
      }
      private def zipWithFnv( ls : Array[Int], rs : Array[Int] ) : Array[Int] = {
        val len = ls.length;
        val out = Array.ofDim[Int]( len );
        var i = 0;
        while (i < len ) {
          out(i) = fnv( ls(i), rs(i) );
          i += 1
        }
        out
      }
      /* overwrites first argument */
      private def zipWithFnv_DESTRUCTIVE( ls : Array[Int], rs : Array[Int] ) : Array[Int] = {
        val len = ls.length;
        var i = 0;
        while (i < len ) {
          ls(i) = fnv( ls(i), rs(i) );
          i += 1
        }
        ls
      }

      private def fnv( v1 : Int, v2 : Int ) : Int = (((UP(v1) * FnvPrime) ^ UP(v2))  & 0xFFFFFFFFL).toInt
    }

    /*
     * We're really keeping this around as documentation.
     * An implementation with UInts ineffectiently packed into LSB of longs (so they look like UInts without extra work),
     * without much in the way of optimization.
     */ 
    class UInt32AsLongUnoptimized extends Manager {
      type Cache       = Array[Array[Long]]
      type Dataset     = Array[Array[Long]]
      type Row         = Array[Long]

      protected implicit val rowClassTag : ClassTag[Row] = LongArrayClassTag;

      /*
       *  we must read rawBytes as 4-byte little-endian unsigned ints, so we pack them into longs
       * 
       *  rawBytes.length must be divisble by four, or ArrayIndexOutOfBoundsException
       */ 
      private def readRow( rawBytes : Array[Byte] ) : Row = Array.range(0, rawBytes.length, 4).map( IntegerUtils.intFromByteArrayLittleEndian( rawBytes, _ ) & 0xFFFFFFFFL )

      /*
       *  we must write the unsigned ints packed into the row's Longs as 4-byte little-endian unsigned ints
       * 
       */ 
      private def writeRow( in : Row ) : Array[Byte] = {
        val inLen = in.length;
        val out = Array.ofDim[Byte]( inLen * 4 );
        var i = 0;
        while ( i < inLen ) {
          IntegerUtils.intIntoByteArrayLittleEndian( in(i).toInt, i * 4, out );
          i += 1
        }
        out
      }

      private def sha3_512_readRow( stuff : Array[Byte] ) : Row = readRow( SHA3_512.rawHash( stuff ) )

      // this seems very arcane...
      protected def mkCache( cacheSize : Long, seed : Array[Byte] ) : Cache = {
        val n = requireValidInt( cacheSize / HashBytes );
        val o = Array.iterate( sha3_512_readRow( seed ), n )( lastLongs => sha3_512_readRow( writeRow( lastLongs ) ) )
        for (_ <- 0L until CacheRounds; i <- 0 until n ) {
          val v = (o(i)(0) +% n).toInt;
          o(i) = sha3_512_readRow( writeRow( zipWithXor(o((i-1+n) % n), o(v)) ) )
        }
        o
      }

      def hashCache( cache : Cache ) : SHA3_256 = SHA3_256.hash( cache.flatMap( writeRow ) )

      //private def fourLittleEndianBytesAsUnsigned( src : Array[Byte], wordOffset : Int ) = IntegerUtils.toUnsigned( IntegerUtils.intFromByteArrayLittleEndian( src, wordOffset * 4 ) );

      protected[pow] def extractDatasetRow( dataset : Dataset, i : Int ) : Row = dataset(i)

      protected[pow] def calcDatasetRow( cache : Cache, i : Int ) : Row = {
        val n = cache.length;
        val r = HashBytesOverWordBytes;

        val mix = {
          val tmp = cache( i % n ).clone;
          tmp(0) = tmp(0) ^ i;
          sha3_512_readRow( writeRow( tmp ) )
        }

        @tailrec
        def remix( lastMix : Row, count : Int = 0) : Row = {
          count match {
            case DatasetParents => lastMix;
            case j              => {
              val cacheIndex = fnv( i ^ j, lastMix( j % r ) );
              val nextMix = zipWithFnv( lastMix, cache( (cacheIndex +% n).toInt ) )
              remix( nextMix, count + 1 )
            }
          }
        }

        sha3_512_readRow( writeRow( remix( mix ) ) )
      }

      protected[pow] def toDataset( array : Array[Row] ) : Dataset = array;

      private def combineIndexedArrays( srcAccessor : Int => Row, srcLen : Int, times : Int ) : Row = {
        val arraylen = requireValidInt( srcLen.toLong * times.toLong );
        val tmp = Array.ofDim[Long]( arraylen );
        (0 until times).foreach( i => System.arraycopy( srcAccessor(i), 0, tmp, i * srcLen, srcLen ) )
        tmp
      }

      private def replicateArray( src : Row, srcLen : Int, times : Int ) : Row = combineIndexedArrays( _ => src, srcLen, times );

      protected def hashimoto( seedBytes : Array[Byte], fullSize : Long, datasetAccessor : Int => Row ) : Hashimoto = {
        val n = fullSize / HashBytes;
        val w = MixBytesOverWordBytes;
        val mixHashes = MixBytesOverHashBytes;

        val s = sha3_512_readRow( seedBytes );
        val len = s.length;

        val startMix : Row = replicateArray(s, len, mixHashes);

        @tailrec
        def remix( lastMix : Row, i : Int = 0 ) : Row = {
          i match {
            case Accesses => lastMix;
            case _        => {
              val p = ( fnv( i ^ s(0), lastMix( i % w ) ) % (n / mixHashes) ) * mixHashes;
              val offsetAccessor : Int => Row = j => datasetAccessor( requireValidInt( p + j ) );

              // note that we are looking up fixed-length hashes
              // all the same length as our seed hash, so len is fine
              val newData = combineIndexedArrays( offsetAccessor, len, mixHashes );
              val nextMix = zipWithFnv( lastMix, newData )
              remix( nextMix, i + 1 )
            }
          }
        }

        val uncompressedMix = remix( startMix );

        def compressNextFour( src : Row, offset : Int ) : Long = {
          def srcval( i : Int ) = src( offset + i );
          fnv( fnv( fnv( srcval(0), srcval(1) ), srcval(2) ), srcval(3) )
        }

        val compressedMix = Array.range(0, uncompressedMix.length, 4).map( i => compressNextFour( uncompressedMix, i ) );

        val mixDigest = writeRow( compressedMix );
        val result = SHA3_256.rawHash( writeRow( s ++ compressedMix ) )

        new Hashimoto( ImmutableArraySeq.Byte( mixDigest ), Unsigned256( BigInt( 1, result ) ) )
      }

      // using Tuple2.zipped.map causes serious memory stress, so this
      private def zipWithXor( ls : Array[Long], rs : Array[Long] ) : Array[Long] = {
        val len = ls.length;
        val out = Array.ofDim[Long]( len );
        var i = 0;
        while (i < len ) {
          out(i) = ls(i) ^ rs(i);
          i += 1
        }
        out
      }
      private def zipWithFnv( ls : Array[Long], rs : Array[Long] ) : Array[Long] = {
        val len = ls.length;
        val out = Array.ofDim[Long]( len );
        var i = 0;
        while (i < len ) {
          out(i) = fnv( ls(i), rs(i) );
          i += 1
        }
        out
      }

      private def fnv( v1 : Long, v2 : Long ) : Long = ((v1 * FnvPrime) ^ v2) & 0xFFFFFFFFL
    }
  }
  trait Manager {

    //abstract members
    type Cache;
    type Dataset;
    type Row;

    protected implicit val rowClassTag : ClassTag[Row];

    protected def mkCache( cacheSize : Long, seed : Array[Byte] ) : Cache;
    protected def calcDatasetRow( cache : Cache, i : Int )        : Row;
    protected def toDataset( array : Array[Row] )                 : Dataset;
    protected def extractDatasetRow( dataset : Dataset, i : Int ) : Row;

    protected def hashimoto( seedBytes : Array[Byte], fullSize : Long, datasetAccessor : Int => Row ) : Hashimoto;

    // public utilities
    def epochFromBlock( blockNumber : Long )         : Long = Ethash23.epochFromBlock( blockNumber );
    def blocksRemainingInEpoch( blockNumber : Long ) : Long = Ethash23.blocksRemainingInEpoch( blockNumber );

    def mkCacheForBlock( blockNumber : Long ) : Cache = mkCacheForEpoch( epochFromBlock( blockNumber ) );
    def calcDatasetForBlock( blockNumber : Long ) : Dataset = calcDatasetForEpoch( epochFromBlock( blockNumber ) );

    def mkCacheForEpoch( epochNumber : Long ) : Cache = {
      val cacheSize = getCacheSizeForEpoch( epochNumber );
      val seed      = Seed.getForEpoch( epochNumber );
      mkCache( cacheSize, seed );
    }

    def calcDatasetForEpoch( epochNumber : Long ) : Dataset = {
      val cache = mkCacheForEpoch( epochNumber );
      val fullSize = getFullSizeForEpoch( epochNumber );
      calcDataset( cache, fullSize )
    }

    def hashimotoLight( header : EthBlock.Header, cache : Cache, nonce : Unsigned64 ) : Hashimoto = {
      val blockNumber = requireValidLong( header.number.widen );
      hashimotoLight( getFullSizeForBlock( blockNumber ), cache, truncatedHeaderHash( header ), nonce )
    }

    def hashimotoFull( header : EthBlock.Header, dataset : Dataset, nonce : Unsigned64 ) : Hashimoto = {
      val blockNumber = requireValidLong( header.number.widen );
      hashimotoFull( getFullSizeForBlock( blockNumber ), dataset, truncatedHeaderHash( header ), nonce )
    }

    // protected utilities
    protected[pow] val isParallel = false;

    protected[pow] def requireValidInt( l : Long )     : Int  = if (l.isValidInt) l.toInt else throw new IllegalArgumentException( s"${l} is not a valid Int, as required." );
    protected[pow] def requireValidLong( bi : BigInt ) : Long = if (bi.isValidLong) bi.toLong else throw new IllegalArgumentException( s"${bi} is not a valid Long, as required." );

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

    protected[pow] def calcDataset( cache : Cache, fullSize : Long ) : Dataset = calcDatasetSequential( cache, fullSize )

    protected[pow] final def calcDatasetSequential( cache : Cache, fullSize : Long ) : Dataset = {
      val len = datasetLen( fullSize )
      val out = (0 until len).toArray.map( calcDatasetRow( cache, _ ) )
      toDataset( out )
    }
    protected[pow] final def calcDatasetParallel( cache : Cache, fullSize : Long ) : Dataset = {
      val len = datasetLen( fullSize )
      val out = (0 until len).toArray.par.map( calcDatasetRow( cache, _ ) ).toArray
      toDataset( out )
    }
    protected[pow] final def datasetLen( fullSize : Long ) : Int = requireValidInt( fullSize / HashBytes );

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

    // private utilities
    private def hashimotoLight( fullSize : Long, cache : Cache, truncatedHeaderHash : SHA3_256, nonce : Unsigned64 ) : Hashimoto = {
      hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => calcDatasetRow( cache, i ) )
    }
    private def hashimotoFull( fullSize : Long, dataset : Dataset, truncatedHeaderHash : SHA3_256, nonce : Unsigned64 ) : Hashimoto = {
      hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => extractDatasetRow( dataset, i ) )
    }
    private def hashimoto(truncatedHeaderHash : SHA3_256, nonce : Unsigned64 , fullSize : Long, datasetAccessor : Int => Row ) : Hashimoto = {
      hashimoto( ( truncatedHeaderHash.bytes ++ nonce.widen.unsignedBytes(8).reverse ).toArray, fullSize, datasetAccessor )
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
