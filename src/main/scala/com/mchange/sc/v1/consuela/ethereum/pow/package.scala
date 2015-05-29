package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela.conf.Config;

package object pow {
  val ThresholdNumerator : BigInt           = BigInt(1) << 256;
  val Ethash23Manager    : ethash23.Manager = if ( Config.EthereumPowFull ) ethash23.Manager.FullStochasticCaching else ethash23.Manager.Light;
}
