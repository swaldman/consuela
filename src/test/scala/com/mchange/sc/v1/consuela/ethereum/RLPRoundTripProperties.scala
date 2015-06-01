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
