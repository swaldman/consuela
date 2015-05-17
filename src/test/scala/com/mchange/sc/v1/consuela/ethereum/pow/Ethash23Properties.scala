package com.mchange.sc.v1.consuela.ethereum.pow;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.encoding._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned64

import com.mchange.sc.v1.consuela.hash.SHA3_256;

/*
 * Values for these tests are taken from the Ethash23 specification (the tables in the appendix)
 * 
 *   https://github.com/ethereum/wiki/wiki/Ethash
 * 
 * And from ethereum TCK tests
 * 
 *   https://github.com/ethereum/tests/blob/develop/PoWTests/ethash_tests.json
 */ 
object Ethash23Properties extends Properties("Ethash23") {

  val Implementation = Ethash23.Default;

  //MT : protected by its own lock
  private val cacheHashes = scala.collection.mutable.HashMap.empty[Long,Implementation.Cache];

  def getCacheForEpoch( epochNumber : Long ) : Implementation.Cache = cacheHashes.synchronized { 
    cacheHashes.getOrElseUpdate( epochNumber, Implementation.mkCacheForEpoch( epochNumber ) ) 
  }

  def getCacheForBlock( blockNumber : Long ) : Implementation.Cache = getCacheForEpoch( Ethash23.epochFromBlock( blockNumber ) );

  property("Zeroth Block (Zeroth Epoch) Cache Size")       = Prop( Implementation.getCacheSizeForBlock(0) == 16776896L );
  property("30000th Block (First Epoch) Cache Size")       = Prop( Implementation.getCacheSizeForBlock(30000) == 16907456L );
  property("2047*30000th Block (2047th Epoch) Cache Size") = Prop( Implementation.getCacheSizeForBlock(2047 * 30000) == 285081536L );

  property("Zeroth Block (Zeroth Epoch) Full Size")       = Prop( Implementation.getFullSizeForBlock(0) == 1073739904L );
  property("30000th Block (First Epoch) Full Size")       = Prop( Implementation.getFullSizeForBlock(30000) == 1082130304L );
  property("2047*30000th Block (2047th Epoch) Full Size") = Prop( Implementation.getFullSizeForBlock(2047 * 30000) == 18245220736L );

  val epochZeroCache = getCacheForEpoch( 0 );

  property("Expected Epoch Zero Cache Hash") = Prop{
    Implementation.hashCache( epochZeroCache ) == SHA3_256.withBytes( "35ded12eecf2ce2e8da2e15c06d463aae9b84cb2530a00b932e4bbc484cde353".decodeHex );
  }

  object Check1 {
    val headerRLP = "f901f3a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347940000000000000000000000000000000000000000a09178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4ea056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008302000080830f4240808080a058f759ede17a706c93f13030328bcea40c1d1341fb26f2facd21ceb0dae57017884242424242424242".decodeHex;
    val header = RLP.decodeComplete[EthBlock.Header]( headerRLP ).right.get;
    val nonce = Unsigned64(0x4242424242424242L);
    val blockNumber = header.number.widen.toLong;
    val cache = getCacheForBlock( blockNumber );
    val truncatedHeaderHash = Implementation.truncatedHeaderHash( header );
    val hashimoto = Implementation.hashimotoLight( header, cache, nonce );
  }

  property("Expected hash of (truncated) first example header (Check1)") = Prop( 
    Check1.truncatedHeaderHash == SHA3_256.withBytes( "2a8de2adf89af77358250bf908bf04ba94a6e8c3ba87775564a41d269a05e4ce".decodeHex ) 
  )
  property("Expected result for first example header (Check1)") = Prop( 
    Check1.hashimoto.result.widen == BigInt(1, "dd47fd2d98db51078356852d7c4014e6a5d6c387c35f40e2875b74a256ed7906".decodeHex)
  )
  property("Expected mixDigest for first example header (Check1)") = Prop( 
    Check1.hashimoto.mixDigest == "58f759ede17a706c93f13030328bcea40c1d1341fb26f2facd21ceb0dae57017".decodeHexAsSeq
  )

  object Check2 {
    val headerRLP = "f901f7a01bef91439a3e070a6586851c11e6fd79bbbea074b2b836727b8e75c7d4a6b698a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d4934794ea3cb5f94fa2ddd52ec6dd6eb75cf824f4058ca1a00c6e51346be0670ce63ac5f05324e27d20b180146269c5aab844d09a2b108c64a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008302004002832fefd880845511ed2a80a0e55d02c555a7969361cf74a9ec6211d8c14e4517930a00442f171bdb1698d17588307692cf71b12f6d".decodeHex;
    val header = RLP.decodeComplete[EthBlock.Header]( headerRLP ).right.get;
    val nonce = Unsigned64(0x307692cf71b12f6dL);
    val blockNumber = header.number.widen.toLong;
    val cache = getCacheForBlock( blockNumber );
    val truncatedHeaderHash = Implementation.truncatedHeaderHash( header );
    val hashimoto = Implementation.hashimotoLight( header, cache, nonce );
  }

  property("Expected hash of (truncated) first example header (Check2)") = Prop( 
    Check2.truncatedHeaderHash == SHA3_256.withBytes( "100cbec5e5ef82991290d0d93d758f19082e71f234cf479192a8b94df6da6bfe".decodeHex ) 
  )
  property("Expected result for first example header (Check2)") = Prop( 
    Check2.hashimoto.result.widen == BigInt( 1, "ab9b13423cface72cbec8424221651bc2e384ef0f7a560e038fc68c8d8684829".decodeHex )
  )
  property("Expected mixDigest for first example header (Check2)") = Prop( 
    Check2.hashimoto.mixDigest == "e55d02c555a7969361cf74a9ec6211d8c14e4517930a00442f171bdb1698d175".decodeHexAsSeq
  )
}

