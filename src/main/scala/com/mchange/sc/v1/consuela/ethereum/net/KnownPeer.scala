package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v3.failable._
import com.mchange.sc.v1.consuela.ethereum.EthPublicKey;
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact32;

object KnownPeer {
  trait Database {
    def seek( peer : EthPublicKey )    : Failable[Option[KnownPeer]]
    def without( peer : EthPublicKey ) : Failable[KnownPeer.Database]
  }
}
case class KnownPeer( sessionToken : ByteSeqExact32 )
