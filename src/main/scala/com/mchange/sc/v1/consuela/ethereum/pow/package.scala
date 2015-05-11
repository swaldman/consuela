package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import hash.SHA3_256;

import com.mchange.sc.v1.consuela.conf.Config;

package object pow {
  implicit val Ethash23SeedPrimer = Ethash23.Seed.Primer(
    Config.EthereumPowEthash23SeedPrimerEpochNumber,
    SHA3_256.withBytes(Config.EthereumPowEthash23SeedPrimerValue.decodeHex)
  );
}
