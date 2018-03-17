package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.{stub, EthPrivateKey}

object Test {
  /**
    * Private key 0x7e57 defines (by convention only) our default testing account,
    * which may be used as a faucet on testnets.
    * 
    * The additional five accounts are just conveniences for testing.
    */ 
  val Sender : IndexedSeq[Sender] = (0x7e57 to 0x7e5c).map( i => stub.Sender.Basic( EthPrivateKey( BigInt(i) ) ) )
}
