package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.ethereum._;
import specification.Types.Unsigned256;
import specification.Arbitraries._;


import org.scalacheck._;
import Arbitrary.arbitrary;

object Arbitraries {
  implicit val ArbitraryEthHash : Arbitrary[EthHash] = Arbitrary {
    Gen.containerOfN[Array,Byte]( EthHashLen, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) ).map( EthHash.withBytes( _ ) )
  }

  implicit val ArbitraryContractAccount : Arbitrary[EthWorldState.Account.Contract] = Arbitrary {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash];
      codeHash    <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Contract( nonce, balance, storageRoot, codeHash )
    }
  }

  implicit val ArbitraryAgentAccount : Arbitrary[EthWorldState.Account.Agent] = Arbitrary {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Agent( nonce, balance, storageRoot )
    }
  }
}
