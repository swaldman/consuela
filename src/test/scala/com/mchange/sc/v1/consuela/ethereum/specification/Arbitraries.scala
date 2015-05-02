package com.mchange.sc.v1.consuela.ethereum.specification;

import org.scalacheck._;
import com.mchange.sc.v2.restrict.scalacheck._;

object Arbitraries {

  def genRandomBytes( n : Int ) : Gen[Array[Byte]] = Gen.containerOfN[Array,Byte]( n, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) );
  private def genUnsignedBigInt( bitLength : Int ) : Gen[BigInt] = {
    require( bitLength % 8 == 0 );

    genRandomBytes( bitLength / 8).map( BigInt(1, _ ) )
  }
  private def arbUnsignedBigInt( bitLength : Int ) : Arbitrary[BigInt] = Arbitrary( genUnsignedBigInt( bitLength ) );

  implicit val ArbitraryUnsignedBigInt  = Types.UnsignedBigInt.arbitrary;
  implicit val ArbitraryUnsigned8       = Types.Unsigned8.arbitrary;
  implicit val ArbitraryUnsigned256     = Types.Unsigned256.arbitrary( arbUnsignedBigInt( 256 ) ); // to reduce the failure rate when producing values
  implicit val ArbitraryUnsigned2048    = Types.Unsigned2048.arbitrary;

  implicit val ArbitrarySignatureR      = Types.SignatureR.arbitrary;
  implicit val ArbitrarySignatureS      = Types.SignatureS.arbitrary;
  implicit val ArbitrarySignatureV      = Types.SignatureV.arbitrary;

  implicit val ArbitraryByteSeqExact4   = Types.ByteSeqExact4.arbitrary;
  implicit val ArbitraryByteSeqExact8   = Types.ByteSeqExact8.arbitrary;
  implicit val ArbitraryByteSeqExact20  = Types.ByteSeqExact20.arbitrary;
  implicit val ArbitraryByteSeqExact32  = Types.ByteSeqExact32.arbitrary;
  implicit val ArbitraryByteSeqExact256 = Types.ByteSeqExact256.arbitrary;

  implicit val ArbitraryByteSeqMax1024  = Types.ByteSeqMax1024.arbitrary;
}
