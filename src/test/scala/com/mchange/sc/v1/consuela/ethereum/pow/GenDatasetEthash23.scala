package com.mchange.sc.v1.consuela.ethereum.pow;

import java.io.{File, FileOutputStream, BufferedOutputStream}

object GenDatasetEthash23 {
  def main( argv : Array[String] ) : Unit = {
    println( s"Generating dataset for epoch ${argv(0)}, will write to file '${argv(1)}'" );

    val Implementation = Ethash23.LoggingParallelUInt32AsInt;
    val dataset = Implementation.calcDatasetForEpoch( argv(0).toLong );
    val destFile = new File( argv(1) );
    println( s"dataset numRows: ${dataset.length}" );
    val numCols = dataset(0).length;
    assert( dataset.forall( _.length == numCols ) );
    println( s"dataset numCols: ${numCols}" );

    val startWrite = System.currentTimeMillis();

    val os = new BufferedOutputStream( new FileOutputStream( destFile ), 16 * 1024 * 1024 );
    try Implementation.writeDagFile( os, dataset ) finally os.close;

    val endWrite = System.currentTimeMillis();
    val secs = (endWrite - startWrite) / 1000d;

    println( s"Wrote '${argv(1)}' (write took ${secs} seconds)" );
  }
}
