package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

import com.mchange.sc.v1.consuela._;

import java.io._;

object ImplementationProperties extends Properties("ethash23.Manager") {

  val FakeDagFileContents = "0xfecaddbaaddee1fe00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff".decodeHex;

  val impl = Implementation.Default;

  property("OneRowDagFileReads") = Prop {
    val bais = new ByteArrayInputStream( FakeDagFileContents );
    val dataset = try impl.readDagFile( bais, Some( FakeDagFileContents.length.toLong ) ) finally bais.close();
    dataset.length == 1
  }

}
