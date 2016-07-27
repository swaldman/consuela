package com.mchange.sc.v1.consuela.ethereum.specification;

import com.mchange.sc.v1.consuela.ethereum.EthereumException

import spire.implicits._ // for ** operator

// from yellow paper, sec 2.1
object Denominations {
  sealed abstract class Denomination( multiplier : scala.BigDecimal ) {
    val unitName : String

    def fromWei( wei : BigInt ) : BigDecimal = {
      BigDecimal( wei ) / multiplier
    }
  }
  final object Wei extends Denomination( Multiplier.BigDecimal.Wei ) {
    val unitName : String = "wei"

    override def fromWei( wei : BigInt ) : BigDecimal = BigDecimal(wei)
  }
  final object Szabo extends Denomination( Multiplier.BigDecimal.Szabo ) {
    val unitName : String = "szabo"
  }
  final object Finney extends Denomination( Multiplier.BigDecimal.Finney ) {
    val unitName : String = "finney"
  }
  final object Ether extends Denomination( Multiplier.BigDecimal.Ether ) {
    val unitName : String = "ether"
  }

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
    final object BigDecimal extends Multiplier[scala.BigDecimal] {
      val Wei    = scala.BigDecimal( BigInt.Wei );
      val Szabo  = scala.BigDecimal( BigInt.Szabo );
      val Finney = scala.BigDecimal( BigInt.Finney );
      val Ether  = scala.BigDecimal( BigInt.Ether );
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
