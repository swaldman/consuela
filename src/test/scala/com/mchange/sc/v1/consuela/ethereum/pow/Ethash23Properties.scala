package com.mchange.sc.v1.consuela.ethereum.pow;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

object Ethash23Properties extends Properties("Ethash23") {

  property("Zeroth Block (Zeroth Epoch) Cache Size")       = Prop( Ethash23.getCacheSizeForBlock(0) == 16776896L );
  property("30000th Block (First Epoch) Cache Size")       = Prop( Ethash23.getCacheSizeForBlock(30000) == 16907456L );
  property("2047*30000th Block (2047th Epoch) Cache Size") = Prop( Ethash23.getCacheSizeForBlock(2047 * 30000) == 285081536L );

  property("Zeroth Block (Zeroth Epoch) Full Size")       = Prop( Ethash23.getFullSizeForBlock(0) == 1073739904L );
  property("30000th Block (First Epoch) Full Size")       = Prop( Ethash23.getFullSizeForBlock(30000) == 1082130304L );
  property("2047*30000th Block (2047th Epoch) Full Size") = Prop( Ethash23.getFullSizeForBlock(2047 * 30000) == 18245220736L );

  import com.mchange.sc.v1.consuela._
  import com.mchange.sc.v1.consuela.ethereum._
  import com.mchange.sc.v1.consuela.ethereum.encoding._
  import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned64

  val data = "f901f3a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347940000000000000000000000000000000000000000a09178d0f23c965d81f0834a4c72c6253ce6830f4022b1359aaebfc1ecba442d4ea056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008302000080830f4240808080a058f759ede17a706c93f13030328bcea40c1d1341fb26f2facd21ceb0dae57017884242424242424242".decodeHex;
  val header = RLP.decodeComplete[EthBlock.Header]( data ).right.get;
  val nonce = Unsigned64(0x4242424242424242L)

  val headerRLP      = data;
  val truncHeaderRLP = {
    val headerElement = RLP.toElement[EthBlock.Header]( header );
    val RLP.Element.Seq( fullSeq ) = headerElement;
    val truncSeq = fullSeq.take(13);
    RLP.Element.encode( RLP.Element.Seq( truncSeq ) )
  };
  val zeroedHeaderRLP = {
    val zeroedHeader = header.copy( mixHash = AllZeroesEthHash, nonce = Unsigned64(0) );
    RLP.encode( zeroedHeader )
  }

  val headerHash = EthHash.hash( truncHeaderRLP ).bytes;

  property("Expected Example headerHash") = Prop( headerHash == "2a8de2adf89af77358250bf908bf04ba94a6e8c3ba87775564a41d269a05e4ce".decodeHexAsSeq );

}

