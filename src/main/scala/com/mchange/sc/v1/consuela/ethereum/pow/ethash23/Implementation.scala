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

import com.mchange.sc.v1.consuela._;
import hash.{Keccak256,Keccak512}

import scala.util.Try;
import scala.annotation.tailrec;
import scala.reflect.ClassTag;

import ethereum._;
import ethereum.encoding.RLP;
import ethereum.specification.Types.{Unsigned64,Unsigned256};

import com.mchange.lang.{LongUtils,IntegerUtils};
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

import com.mchange.sc.v2.failable._;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import com.mchange.sc.v1.consuela.ethereum.pow.ethash23; //so that we can refer to the package object explicitly

import java.nio.file.{Files,Path}
import java.io.{BufferedInputStream,BufferedOutputStream,File,FileInputStream,InputStream,OutputStream,EOFException}

// is Long math good enough? (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...
// but when the dataset gets bigger than ~8GB, we'll exceed int array indices, and have to encode
// values into fully packed longs, rather than ints or longs representing unsigned ints as currently
// implements. (the current implementation will fail fast when this limit is exceeded.)

object Implementation {
  private implicit lazy val logger = MLogger( this );

  lazy val Default = ParallelUInt32AsInt;

  // learn something new every day! mark nested objects as final
  // http://stackoverflow.com/questions/30265070/whats-the-point-of-nonfinal-singleton-objects-in-scala  
  final object SequentialUInt32AsInt extends UInt32AsInt;
  final object ParallelUInt32AsInt extends UInt32AsInt with Parallel;
  final object LoggingSequentialUInt32AsInt extends UInt32AsInt with Logging;
  final object LoggingParallelUInt32AsInt extends UInt32AsInt with Parallel with Logging;

  final object Monitor {
    final object Factory {
      implicit final object NoOp extends Factory {
        def create : Monitor = Monitor.NoOp;
      }
    }
    trait Factory {
      def create : Monitor;
    }
    abstract class Abstract extends Monitor {
      def start( rowCount : Long )      : Unit = {}
      def rowHandled( rowIndex : Long ) : Unit = {};
      def completed                     : Unit = {};
    }
    final object NoOp extends Abstract;
  }
  trait Monitor { // no no-op defaults to keep this a Java 7 compatible interface
    def start( rowCount : Long )      : Unit;
    def rowHandled( rowIndex : Long ) : Unit;
    def completed                     : Unit;
  }

  trait Parallel extends Implementation {
    override def doCalcDataset( cache : Cache, fullSize : Long )( mf : Monitor.Factory ) : Dataset = calcDatasetParallel( cache, fullSize )( mf )
  }
  trait Logging extends Implementation { 
    lazy val parModifier = if ( this.isInstanceOf[Parallel] ) "parallel" else "sequential";

    override def mkCacheForEpoch( epochNumber : Long ) : Cache = {
      val startBlock = epochNumber * EpochLength;
      val lastBlock  = startBlock + EpochLength - 1;
      INFO.log( s"Beginning computation of cache for epoch ${epochNumber} (blocks ${startBlock} thru ${lastBlock})" );
      val start = System.currentTimeMillis();
      val out = super.mkCacheForEpoch( epochNumber );
      val done = System.currentTimeMillis();
      val secs = ( done - start ) / 1000d
      INFO.log( s"Completed computation of cache for epoch ${epochNumber} (blocks ${startBlock} thru ${lastBlock}) in ${secs} seconds" );
      out
    }

    override def calcDataset( cache : Cache, fullSize : Long )( implicit mf : Monitor.Factory ) : Dataset = {
      INFO.log( s"Beginning ${parModifier} computation of dataset, fullSize=${fullSize}, rows=${datasetLen(fullSize)}" );
      val start = System.currentTimeMillis();
      val out = super.calcDataset( cache, fullSize )( mf );
      val done = System.currentTimeMillis();
      val secs = ( done - start ) / 1000d
      INFO.log( s"Completed ${parModifier} computation of dataset in ${secs} seconds, fullSize=${fullSize}, rows=${datasetLen(fullSize)}" );
      out
    }
    abstract override protected[ethash23] def calcDatasetRow( cache : Cache, i : Int )  : Row = {
      val out = super.calcDatasetRow( cache : Cache, i : Int );
      INFO.log( s"Computed dataset row #${i}" )
      out
    }
  }

  class UInt32AsInt extends Implementation {
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
    protected def writeRow( in : Row ) : Array[Byte] = {
      val inLen = in.length;
      val out = Array.ofDim[Byte]( inLen * 4 );
      var i = 0;
      while ( i < inLen ) {
        IntegerUtils.intIntoByteArrayLittleEndian( in(i), i * 4, out );
        i += 1
      }
      out
    }

    private def keccak_512_readRow( stuff : Array[Byte] ) : Row = readRow( Keccak512.rawHash( stuff ) )

    private def keccak_512_roundTrip( start : Array[Int] ) : Row = readRow( Keccak512.rawHash( writeRow( start ) ) )

    private def keccak_512_roundTrip_DESTRUCTIVE( start : Array[Int] ) : Row = {
      val asBytes = writeRow( start );
      val asHash = Keccak512.rawHash( asBytes )

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
      val o = Array.iterate( keccak_512_readRow( seed ), n )( last => keccak_512_readRow( writeRow( last ) ) )
      for (_ <- 0L until CacheRounds; i <- 0 until n ) {
        val v = (UP( o(i)(0) ) +% n).toInt;
        o(i) = keccak_512_roundTrip_DESTRUCTIVE( zipWithXor(o( (i-1+n) % n ), o(v)) )
      }
      o
    }

    def hashCache( cache : Cache ) : Keccak256 = Keccak256.hash( cache.flatMap( writeRow ) )

    def dumpDatasetBytes( os : OutputStream, dataset : Dataset ) : Unit = dataset.foreach( iarr => os.write( writeRow( iarr ) ) )

    // this is ugly, but since these files are gigantic, i'm avoiding abstractions that
    // might require preloading or carry much overhead per byte or row
    def readDatasetBytes( is : InputStream, mbDatasetLen : Option[Long] ) : Dataset = {
      val rowLen = RowWidth * 4;
      val bufferLen = rowLen;
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
              if (b < 0) throw new EOFException( s"Unexpected EOF reading byte ${i} of Dataset row! (should be ${bufferLen} bytes)" );
            }
          }
          // ok, we've filled the buffer, now we just have to interpret
          readRow( buffer )
        }
      }

      val initRows = requireValidInt( mbDatasetLen.fold( DefaultDatasetLen )( identity ) / rowLen );
      val arrays = new scala.collection.mutable.ArrayBuffer[Row]( initRows );
      var row = handleRow;
      while ( row != null ) {
        arrays += row;
        row = handleRow;
      }
      arrays.toArray
    }

    private val DefaultDatasetLen = 2L * 1024 * 1024 * 1024;

    // unsigned promote
    private def UP( i : Int ) : Long = i & 0xFFFFFFFFL

    protected[ethash23] def extractDatasetRow( dataset : Dataset, i : Int ) : Row = dataset(i)

    protected[ethash23] def calcDatasetRow( cache : Cache, i : Int ) : Row = {
      val n = cache.length;
      val r = HashBytesOverWordBytes;

      val mix = {
        val tmp = cache( i % n ).clone;
        tmp(0) = tmp(0) ^ i;
        keccak_512_roundTrip_DESTRUCTIVE( tmp )
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

      keccak_512_roundTrip_DESTRUCTIVE( remix( mix ) )
    }

    protected[ethash23] def toDataset( array : Array[Row] ) : Dataset = array;

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

      val s = keccak_512_readRow( seedBytes );
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
      val result = Keccak256.rawHash( writeRow( s ++ compressedMix ) )

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
   *  OK, so this is some terrible code reuse, but I couldn't figure out a good way to write this performance-sensitive,
   *  primitive-based code generically without boxing and with decent syntax. I did try! see
   * 
   *      https://github.com/swaldman/consuela/blob/abortive-generic-ethash-manager/src/main/scala/com/mchange/sc/v1/consuela/ethereum/pow/Ethash23.scala 
   *      https://github.com/swaldman/consuela/commit/a4e69183abfeaef1aa5717fd715824c1438d2a5f
   * 
   *  for an abortive attempt.

   * We're really keeping this around as documentation.
   * This is an implementation with UInts ineffectiently packed into LSB of longs (so they look like UInts without extra work),
   * without much in the way of optimization.
   */ 
  class UInt32AsLongUnoptimized extends Implementation {
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
    protected def writeRow( in : Row ) : Array[Byte] = {
      val inLen = in.length;
      val out = Array.ofDim[Byte]( inLen * 4 );
      var i = 0;
      while ( i < inLen ) {
        IntegerUtils.intIntoByteArrayLittleEndian( in(i).toInt, i * 4, out );
        i += 1
      }
      out
    }

    private def keccak_512_readRow( stuff : Array[Byte] ) : Row = readRow( Keccak512.rawHash( stuff ) )

    // this seems very arcane...
    protected def mkCache( cacheSize : Long, seed : Array[Byte] ) : Cache = {
      val n = requireValidInt( cacheSize / HashBytes );
      val o = Array.iterate( keccak_512_readRow( seed ), n )( lastLongs => keccak_512_readRow( writeRow( lastLongs ) ) )
      for (_ <- 0L until CacheRounds; i <- 0 until n ) {
        val v = (o(i)(0) +% n).toInt;
        o(i) = keccak_512_readRow( writeRow( zipWithXor(o((i-1+n) % n), o(v)) ) )
      }
      o
    }

    def hashCache( cache : Cache ) : Keccak256 = Keccak256.hash( cache.flatMap( writeRow ) )

    //private def fourLittleEndianBytesAsUnsigned( src : Array[Byte], wordOffset : Int ) = IntegerUtils.toUnsigned( IntegerUtils.intFromByteArrayLittleEndian( src, wordOffset * 4 ) );

    protected[ethash23] def extractDatasetRow( dataset : Dataset, i : Int ) : Row = dataset(i)

    protected[ethash23] def calcDatasetRow( cache : Cache, i : Int ) : Row = {
      val n = cache.length;
      val r = HashBytesOverWordBytes;

      val mix = {
        val tmp = cache( i % n ).clone;
        tmp(0) = tmp(0) ^ i;
        keccak_512_readRow( writeRow( tmp ) )
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

      keccak_512_readRow( writeRow( remix( mix ) ) )
    }

    protected[ethash23] def toDataset( array : Array[Row] ) : Dataset = array;

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

      val s = keccak_512_readRow( seedBytes );
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
      val result = Keccak256.rawHash( writeRow( s ++ compressedMix ) )

      new Hashimoto( ImmutableArraySeq.Byte( mixDigest ), Unsigned256( BigInt( 1, result ) ) )
    }

    protected def dumpDatasetBytes( os : OutputStream, dataset : Dataset ) : Unit = dataset.foreach( row => os.write( writeRow( row ) ) );

    // this is ugly, but since these files are gigantic, i'm avoiding abstractions that
    // might require preloading or carry much overhead per byte or row
    protected def readDatasetBytes( is : InputStream, mbDatasetLen : Option[Long] ) : Dataset = {

      val rowLen = RowWidth * 4;
      val bufferLen = rowLen;
      val buffer = Array.ofDim[Byte]( bufferLen );

      def handleRow : Array[Long] = {
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
              if (b < 0) throw new EOFException("Unexpected EOF reading byte ${i} of Dataset row! (should be ${bufferLen} bytes)");
            }
          }
          // ok, we've filled the buffer, now we just have to interpret
          readRow( buffer )
        }
      }

      val initRows = requireValidInt( mbDatasetLen.fold( DefaultDatasetLen )( identity ) / rowLen );
      val arrays = new scala.collection.mutable.ArrayBuffer[Row]( initRows );
      var row = handleRow;
      while ( row != null ) {
        arrays += row;
        row = handleRow;
      }
      arrays.toArray
    }

    private val DefaultDatasetLen = 2L * 1024 * 1024 * 1024;

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
trait Implementation {
  import Implementation.Monitor;

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

  protected def dumpDatasetBytes( os : OutputStream, dataset : Dataset ) : Unit;
  protected def readDatasetBytes( is : InputStream, mbInitSize : Option[Long] ) : Dataset;

  protected def writeRow( row : Row ) : Array[Byte];

  // public utilities
  def epochFromBlock( blockNumber : Long )         : Long = ethash23.epochFromBlock( blockNumber );
  def blocksRemainingInEpoch( blockNumber : Long ) : Long = ethash23.blocksRemainingInEpoch( blockNumber );

  def mkCacheForBlock( blockNumber : Long ) : Cache = mkCacheForEpoch( epochFromBlock( blockNumber ) );
  def calcDatasetForBlock( blockNumber : Long )( implicit mf : Monitor.Factory ) : Dataset = calcDatasetForEpoch( epochFromBlock( blockNumber ) )( mf );

  def mkCacheForEpoch( epochNumber : Long ) : Cache = {
    val cacheSize = getCacheSizeForEpoch( epochNumber );
    val seed      = Seed.getForEpoch( epochNumber );
    mkCache( cacheSize, seed );
  }

  def calcDatasetForEpoch( epochNumber : Long )( implicit mf : Monitor.Factory ) : Dataset = {
    val cache = mkCacheForEpoch( epochNumber );
    val fullSize = getFullSizeForEpoch( epochNumber );
    calcDataset( cache, fullSize )( mf )
  }

  def hashimotoLight( header : EthBlock.Header, cache : Cache, nonce : Unsigned64 ) : Hashimoto = {
    val blockNumber = requireValidLong( header.number.widen );
    hashimotoLight( getFullSizeForBlock( blockNumber ), cache, truncatedHeaderHash( header ), nonce )
  }

  def hashimotoFull( header : EthBlock.Header, dataset : Dataset, nonce : Unsigned64 ) : Hashimoto = {
    val blockNumber = requireValidLong( header.number.widen );
    hashimotoFull( getFullSizeForBlock( blockNumber ), dataset, truncatedHeaderHash( header ), nonce )
  }

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

  def calcDataset( cache : Cache, fullSize : Long )( implicit mf : Monitor.Factory ) : Dataset = doCalcDataset( cache, fullSize )( mf )

  /*
   * omit the last two elements, 
   * convert truncated header to RLP, 
   * take Keccak256 hash
   */ 
  def truncatedHeaderHash( header : EthBlock.Header ) : Keccak256 = {
    val headerElement = RLP.toElement[EthBlock.Header]( header );
    val RLP.Element.Seq( fullSeq ) = headerElement;
    val numToKeep = fullSeq.length - 2;
    val truncSeq = fullSeq.take( numToKeep );
    val truncHeaderRLP = RLP.Element.encode( RLP.Element.Seq( truncSeq ) )
    Keccak256.hash( truncHeaderRLP )
  }

  // for memoization / caching to files
  def writeDagFile( os : OutputStream, dataset : Dataset ) : Unit = {
    os.write( DagFile.MagicNumberLittleEndianBytes );
    dumpDatasetBytes( os, dataset );
  }

  def readDagFile( is : InputStream, mbFileLength : Option[Long] ) : Dataset = {
    val checkBytes = Array.ofDim[Byte](DagFile.MagicNumberLittleEndianBytes.length);
    (0 until 8).foreach( i => checkBytes(i) = is.read.toByte );
    val startsWithMagicNumber = DagFile.MagicNumberLittleEndianBytes.zip( checkBytes ).forall( tup => tup._1 == tup._2 );
    if (! startsWithMagicNumber ) throw new DagFile.BadMagicNumberException( s"Found 0x${checkBytes.hex}, should be 0x${DagFile.MagicNumberLittleEndianBytes.hex}" );

    readDatasetBytes( is, mbFileLength.map( len => len - DagFile.MagicNumberLittleEndianBytes.length ) )
  }

  def streamDatasetAsDagFile( os : OutputStream, cache : Cache, fullSize : Long )( implicit mf : Monitor.Factory ) : Unit = {
    val monitor = mf.create;
    val len = datasetLen( fullSize );
    monitor.start( len );
    os.write( DagFile.MagicNumberLittleEndianBytes );
    (0 until len).foreach( i => os.write( writeRow( monitoredCalcDatasetRow( cache, i )( monitor ) ) ) )
    monitor.completed;
  }

  def loadDagFile( seed : Array[Byte] ) : Failable[Dataset] = {
    val file = DagFile.fileForSeed( seed );
    if ( file.exists() && file.canRead() ) {
      Try {
        val is = new BufferedInputStream( new FileInputStream( file ), DagFile.BufferSize );
        try this.readDagFile( is, Some( file.length ) ) finally is.close
      }.toFailable
    } else {
      fail( s"Failed to read DAG from ${file}; The file does not exist or is unreadble." );
    }
  }
  def cacheDataset( seed : Array[Byte], dataset : Dataset ) : Failable[Unit] = {
    val file = DagFile.fileForSeed( seed );
    val path = file.toPath;

    def writeFile : Failable[Unit] = {
      Try {
        val os = new BufferedOutputStream( Files.newOutputStream( path ), DagFile.BufferSize );
        try { this.writeDagFile( os, dataset ); }
        catch {
          case t : Throwable => {
            Files.delete( path );
            throw t;
          }
        }
        finally {
          os.close
        }
      }.toFailable
    }

    ensureCacheDirectory.flatMap( _ => touchDagFile( path ).flatMap( _ => writeFile ) );
  }

  def precomputeCacheDatasetForBlockNumber( blockNumber : Long )( implicit mf : Monitor.Factory ) : Failable[Unit] = {
    precomputeCacheDatasetForEpochNumber( epochFromBlock( blockNumber ) )( mf );
  }
  def precomputeCacheDatasetForEpochNumber( epochNumber : Long )( implicit mf : Monitor.Factory ) : Failable[Unit] = {
    val seed = Seed.getForEpoch( epochNumber );
    val dataset = calcDatasetForEpoch( epochNumber )( mf );
    cacheDataset( seed, dataset )
  }

  def streamDagFileForBlockNumber( blockNumber : Long )( implicit mf : Monitor.Factory ) : Failable[Unit] = streamDagFileForEpochNumber( epochFromBlock( blockNumber ) )( mf );
  def streamDagFileForBlockNumber( blockNumber : Long, file : Option[File] )( implicit mf : Monitor.Factory ) : Failable[Unit] = {
    streamDagFileForEpochNumber( epochFromBlock( blockNumber ), file )( mf );
  }
  def streamDagFileForEpochNumber( epochNumber : Long )( implicit mf : Monitor.Factory ) : Failable[Unit] = streamDagFileForEpochNumber( epochNumber, None )( mf )
  def streamDagFileForEpochNumber( epochNumber : Long, mbFile : Option[File] )( implicit mf : Monitor.Factory ) : Failable[Unit] = {
    streamDagFileForEpochNumber( epochNumber, None, None, mbFile )( mf )
  }
  def streamDagFileForEpochNumber( 
    epochNumber : Long, 
    mbSeed : Option[Array[Byte]],
    mbCache : Option[Cache], 
    mbFile : Option[File] 
  )( implicit mf : Monitor.Factory ) : Failable[Unit] = {
    // this is intended to be callable for arbitary epochs, so we build a light record,
    // which we don't use to update our member record
    val seed      = mbSeed.getOrElse( Seed.getForEpoch( epochNumber ) );
    val cache     = mbCache.getOrElse( this.mkCacheForEpoch( epochNumber ) );
    val failableFile = mbFile.fold( dagFileReadyToWrite( seed ) )( succeed(_) )
    failableFile.flatMap( f => streamDagFileForEpochNumber( epochNumber, cache, f )( mf ) )
  }

  // protected utilities
  protected[ethash23] val isParallel = false;

  protected[ethash23] def requireValidInt( l : Long )     : Int  = if (l.isValidInt) l.toInt else throw new IllegalArgumentException( s"${l} is not a valid Int, as required." );
  protected[ethash23] def requireValidLong( bi : BigInt ) : Long = if (bi.isValidLong) bi.toLong else throw new IllegalArgumentException( s"${bi} is not a valid Long, as required." );

  protected[ethash23] def doCalcDataset( cache : Cache, fullSize : Long )( mf : Monitor.Factory ) : Dataset = calcDatasetSequential( cache, fullSize )( mf )

  protected[ethash23] final def calcDatasetSequential( cache : Cache, fullSize : Long )( mf : Monitor.Factory ) : Dataset = {
    val monitor = mf.create;
    val len = datasetLen( fullSize )
    monitor.start( len );
    val out = (0 until len).toArray.map( monitoredCalcDatasetRow( cache, _ )( monitor ) )
    monitor.completed;
    toDataset( out )
  }
  protected[ethash23] final def calcDatasetParallel( cache : Cache, fullSize : Long )( mf : Monitor.Factory ) : Dataset = {
    val monitor = mf.create;
    val len = datasetLen( fullSize )
    monitor.start( len );
    val out = (0 until len).toArray.par.map( monitoredCalcDatasetRow( cache, _ )( monitor ) ).toArray
    monitor.completed;
    toDataset( out )
  }
  protected[ethash23] final def datasetLen( fullSize : Long ) : Int = requireValidInt( fullSize / HashBytes );

  // private utilities
  private def monitoredCalcDatasetRow( cache : Cache, i : Int )( monitor : Monitor ) : Row = {
    val out = calcDatasetRow( cache, i );
    monitor.rowHandled( i );
    out
  }

  private def hashimotoLight( fullSize : Long, cache : Cache, truncatedHeaderHash : Keccak256, nonce : Unsigned64 ) : Hashimoto = {
    hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => calcDatasetRow( cache, i ) )
  }
  private def hashimotoFull( fullSize : Long, dataset : Dataset, truncatedHeaderHash : Keccak256, nonce : Unsigned64 ) : Hashimoto = {
    hashimoto( truncatedHeaderHash, nonce, fullSize, (i : Int) => extractDatasetRow( dataset, i ) )
  }
  private def hashimoto(truncatedHeaderHash : Keccak256, nonce : Unsigned64 , fullSize : Long, datasetAccessor : Int => Row ) : Hashimoto = {
    hashimoto( ( truncatedHeaderHash.bytes ++ nonce.widen.unsignedBytes(8).reverse ).toArray, fullSize, datasetAccessor )
  }

  // we probably want to optimize this someday
  private def isPrime( num : Long ) : Boolean = {
    def naiveIsPrime : Boolean = {
      val limit = scala.math.sqrt( num ).ceil.toLong;
      var check = 2L;
      while (check <= limit) if( num % check == 0 ) return false else check += 1;
      return true;
    }
    def probablePrime = BigInt(num).isProbablePrime( ProbablePrimeCertainty )

    probablePrime && naiveIsPrime
  }

  private def ensureCacheDirectory : Failable[File] = {
    Try {
      val dir = new File( DagFile.ConfiguredDirectory );
      if (! dir.exists() ) {
        dir.mkdirs();
        if ( DagFile.isPosix ) {
          Files.setPosixFilePermissions( dir.toPath, DagFile.PosixCacheDirPermissions )
        }
      }
      dir
    }.toFailable
  }

  private def dagFileReadyToWrite( seed : Array[Byte] ) : Failable[File] = ensureCacheDirectory.map( _ => DagFile.fileForSeed( seed ) )

  private def touchDagFile( path : Path ) : Failable[Path] = {
    Try {
      Files.newOutputStream( path ).close();
      Files.setPosixFilePermissions( path, DagFile.PosixCacheFilePermissions );
      path
    }.toFailable
  }

  private def streamDagFileForEpochNumber( epochNumber : Long, cache : Cache, file : File )( mf : Monitor.Factory ) : Failable[Unit] = {
    def doStream( path : Path ) : Failable[Unit] = {
      Try {
        val os = new BufferedOutputStream( Files.newOutputStream( path ), DagFile.BufferSize );
        try        { streamDatasetAsDagFile( os, cache, this.getFullSizeForEpoch( epochNumber ) )( mf ) }
        catch      { case t : Throwable => { Files.delete( path ); throw t; } } // don't forget to rethrow into the Try!
        finally    { os.close }
      }.toFailable
    }

    touchDagFile( file.toPath ).flatMap( doStream )
  }
}

