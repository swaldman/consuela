package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela.hash.SHA3_256;

import com.mchange.sc.v1.consuela._;
import conf.Config;
import ethereum.specification.Types.Unsigned256;

import scala.collection._;
import scala.reflect.ClassTag;

// is Long math good enough? (looking at the magnitudes, i think it is, but i'm not certain)
// we can put the whole class in terms of spire SafeLong easily enough if not...
// but when the dataset gets bigger than ~8GB, we'll exceed int array indices, and have to encode
// values into fully packed longs, rather than ints or longs representing unsigned ints as currently
// implements. (the current implementation will fail fast when this limit is exceeded.)

/**
  * implementing REVISION 23 of Ethash spec, defined https://github.com/ethereum/wiki/wiki/Ethash
  */ 
package object ethash23 {
  private[ethash23] final val WordBytes          = 1 << 2;  // 4
  private[ethash23] final val DatasetBytesInit   = 1 << 30; // 2 ** 30
  private[ethash23] final val DatasetBytesGrowth = 1 << 23; // 2 ** 23
  private[ethash23] final val CacheBytesInit     = 1 << 24; // 2 ** 24
  private[ethash23] final val CacheBytesGrowth   = 1 << 17; // 2 ** 17
  private[ethash23] final val CacheMultiplier    = 1 << 10; // 1024
  private[ethash23] final val EpochLength        = 30000;   // 30000
  private[ethash23] final val MixBytes           = 1 << 7;  // 128
  private[ethash23] final val HashBytes          = 1 << 6;  // 64
  private[ethash23] final val DatasetParents     = 1 << 8;  // 256
  private[ethash23] final val CacheRounds        = 3;       // 3
  private[ethash23] final val Accesses           = 1 << 6;  // 64

  private[ethash23] final val DoubleHashBytes = 2 * HashBytes; //128
  private[ethash23] final val DoubleMixBytes  = 2 * HashBytes; //256

  private[ethash23] final val HashBytesOverWordBytes = HashBytes / WordBytes; //16
  private[ethash23] final val MixBytesOverWordBytes  = MixBytes / WordBytes;  //32
  private[ethash23] final val MixBytesOverHashBytes  = MixBytes / HashBytes;  //2

  private[ethash23] final val Revision = 23; // the version of the Ethash spec we are implementing

  private[ethash23] final val FnvPrime = 0x01000193;

  private[ethash23] final val ProbablePrimeCertainty : Int = 8; //arbitrary, tune for performance...

  private[ethash23] final val RowWidth = 16; // four-byte Ints

  private[ethash23] implicit val SeedPrimer = Seed.Primer(
    Config.EthereumPowEthash23SeedPrimerEpochNumber,
    SHA3_256.withBytes(Config.EthereumPowEthash23SeedPrimerValue.decodeHex)
  );

  private[ethash23] implicit final class PlusModInt( val i : Int ) extends AnyVal {
    def +%( other : Int ) = scala.math.abs( i % other )
  }
  private[ethash23] implicit final class PlusModLong( val l : Long ) extends AnyVal {
    def +%( other : Int ) : Long  = scala.math.abs( l % other )
    def +%( other : Long ) : Long = scala.math.abs( l % other )
  }

  def epochFromBlock( blockNumber : Long ) : Long = ( blockNumber / EpochLength )
  def blocksRemainingInEpoch( blockNumber : Long ) : Long = EpochLength - ( blockNumber % EpochLength )

  case class Hashimoto( val mixDigest : immutable.Seq[Byte], val result : Unsigned256 ) {
    override def toString : String = s"Hashimote(mixDigest=${mixDigest.hex},result=${result.widen.unsignedBytes(32).hex})"
  }

  // for reasons I don't quite understand, embedding these implicitlys in object initializers directly,
  // lazy or not, led to errors in initialization ( null pointer or stack overflow). by trial and error,
  // this lazy computation in the parent object resolves the problem
  lazy val IntArrayClassTag  = implicitly[ClassTag[Array[Int]]];
  lazy val LongArrayClassTag = implicitly[ClassTag[Array[Long]]];
}
