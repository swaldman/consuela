package com.mchange.sc.v1.consuela.ethereum.specification;

import com.mchange.sc.v1.consuela.ethereum.EthereumException

import spire.implicits._ // for ** operator

// from yellow paper, sec 2.1
object Denominations {
  final object Multiplier {
    final object Long extends Multiplier[scala.Long] {
      val Wei    = 10L ** 0;
      val Szabo  = 10L ** 12;
      val Finney = 10L ** 15;
      val Ether  = 10L ** 18;
    }
    final object BigInt extends Multiplier[scala.BigInt] {
      val Wei    = scala.BigInt( Long.Wei );
      val Szabo  = scala.BigInt( Long.Szabo );
      val Finney = scala.BigInt( Long.Finney );
      val Ether  = scala.BigInt( Long.Ether );
    }
  }
  trait Multiplier[T] {
    val Wei    : T
    val Szabo  : T
    val Finney : T
    val Ether  : T

    def apply( caseInsensitiveUnitName : String ): T = {
      caseInsensitiveUnitName.toLowerCase match {
        case "wei"    => Wei
        case "szabo"  => Szabo
        case "finney" => Finney
        case "ether"  => Ether
        case _        => throw new EthereumException( s"Unknown unit: '${caseInsensitiveUnitName}'" )
      }
    }
  }
  implicit class LongEther( val value : Long ) extends AnyVal {
    def wei    = value * Multiplier.Long.Wei;
    def szabo  = value * Multiplier.Long.Szabo;
    def finney = value * Multiplier.Long.Finney;
    def ether  = value * Multiplier.Long.Ether;
  }
  implicit class BigIntEther( val value : BigInt ) extends AnyVal {
    def wei    = value * Multiplier.BigInt.Wei;
    def szabo  = value * Multiplier.BigInt.Szabo;
    def finney = value * Multiplier.BigInt.Finney;
    def ether  = value * Multiplier.BigInt.Ether;
  }
}
