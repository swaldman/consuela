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

  val GenContractAccount = {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash];
      codeHash    <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Contract( nonce, balance, storageRoot, codeHash )
    }
  }
  val GenAgentAccount = {
    for {
      nonce       <- arbitrary[Unsigned256];
      balance     <- arbitrary[Unsigned256];
      storageRoot <- arbitrary[EthHash]
    } yield {
      EthWorldState.Account.Agent( nonce, balance, storageRoot )
    }
  }

  implicit val ArbitraryContractAccount : Arbitrary[EthWorldState.Account.Contract] = Arbitrary( GenContractAccount );

  implicit val ArbitraryAgentAccount : Arbitrary[EthWorldState.Account.Agent] = Arbitrary( GenAgentAccount );

  implicit val ArbitraryAccount : Arbitrary[EthWorldState.Account] = Arbitrary( Gen.oneOf( GenContractAccount, GenAgentAccount ) );

}
