package com.mchange.sc.v1.consuela.ethereum.pow;

object GenDatasetEthash23 {
  def main( argv : Array[String] ) : Unit = {
    val dataset = Ethash23.Manager.LoggingParallelUInt32AsInt.calcDatasetForEpoch( argv(0).toLong );
  }
}
