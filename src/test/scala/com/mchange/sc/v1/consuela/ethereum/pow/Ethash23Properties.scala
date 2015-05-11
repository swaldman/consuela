package com.mchange.sc.v1.consuela.ethereum.pow;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

object Ethash23Properties extends Properties("Ethash23") {

  property("Zeroth Block (Zeroth Epoch) Cache Size")       = Prop( Ethash23.getCacheSize(0) == 16776896L );
  property("30000th Block (First Epoch) Cache Size")       = Prop( Ethash23.getCacheSize(30000) == 16907456L );
  property("2047*30000th Block (2047th Epoch) Cache Size") = Prop( Ethash23.getCacheSize(2047 * 30000) == 285081536L );

  property("Zeroth Block (Zeroth Epoch) Full Size")       = Prop( Ethash23.getFullSize(0) == 1073739904L );
  property("30000th Block (First Epoch) Full Size")       = Prop( Ethash23.getFullSize(30000) == 1082130304L );
  property("2047*30000th Block (2047th Epoch) Full Size") = Prop( Ethash23.getFullSize(2047 * 30000) == 18245220736L );


}

