package com.mchange.sc.v1.consuela.ethereum;

import encoding.{RLP,RLPSerializing};

import Arbitraries._;
import specification.Arbitraries._;

import org.scalacheck.{Arbitrary,Prop,Properties};

object RLPRoundTripProperties extends Properties("RLP Roundtrips") {

  def roundTrips[T]( implicit ser : RLPSerializing[T], arb : Arbitrary[T] ) : Prop = Prop.forAll { ( entity : T ) =>
    RLP.decodeComplete[T]( RLP.encode[T]( entity ) ).get == entity
  }

  property("EthAddress")      = roundTrips[EthAddress];
  property("EthTranscation")  = roundTrips[EthTransaction];
  property("EthBlock.Header") = roundTrips[EthBlock.Header];
  property("EthBlock")        = roundTrips[EthBlock];

  property("EthWorldState.Account.Contract" ) = Prop.forAll { ( acctContract : EthWorldState.Account.Contract ) =>
    RLP.decodeComplete[EthWorldState.Account]( RLP.encode[EthWorldState.Account]( acctContract ) ).get == acctContract
  }
  property("EthWorldState.Account.Agent" ) = Prop.forAll { ( acctAgent : EthWorldState.Account.Agent ) =>
    RLP.decodeComplete[EthWorldState.Account]( RLP.encode[EthWorldState.Account]( acctAgent ) ).get == acctAgent
  }
  property("EthWorldState.Account" ) = Prop.forAll { ( acct : EthWorldState.Account ) =>
    RLP.decodeComplete[EthWorldState.Account]( RLP.encode[EthWorldState.Account]( acct ) ).get == acct
  }

}
