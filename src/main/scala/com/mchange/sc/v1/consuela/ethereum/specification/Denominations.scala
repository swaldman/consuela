package com.mchange.sc.v1.consuela.ethereum.specification;

import scala.language.implicitConversions
import com.mchange.sc.v1.consuela.ethereum.EthereumException

// from yellow paper, sec 2.1
object Denominations {
  sealed abstract class Denomination( val multiplier : scala.BigDecimal ) {
    val unitName : String

    def fromWei( wei : BigInt ) : BigDecimal = {
      BigDecimal( wei ) / multiplier
    }
  }
  final object Wei extends Denomination( Multiplier.BigDecimal.Wei ) {
    val unitName : String = "wei"

    override def fromWei( wei : BigInt ) : BigDecimal = BigDecimal(wei)
  }
  final object GWei extends Denomination( Multiplier.BigDecimal.GWei ) {
    val unitName : String = "gwei"
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
  val All : List[Denomination] = Wei :: GWei :: Szabo :: Finney :: Ether :: Nil

  final object Multiplier {
    final object Long extends Multiplier[scala.Long] {
      val Wei    = Math.pow(10, 0).toLong;
      val GWei   = Math.pow(10, 9).toLong;
      val Szabo  = Math.pow(10, 12).toLong;
      val Finney = Math.pow(10, 15).toLong;
      val Ether  = Math.pow(10, 18).toLong;
    }
    final object BigInt extends Multiplier[scala.BigInt] {
      val Wei    = scala.BigInt( Long.Wei );
      val GWei   = scala.BigInt( Long.GWei );
      val Szabo  = scala.BigInt( Long.Szabo );
      val Finney = scala.BigInt( Long.Finney );
      val Ether  = scala.BigInt( Long.Ether );
    }
    final object BigDecimal extends Multiplier[scala.BigDecimal] {
      val Wei    = scala.BigDecimal( BigInt.Wei );
      val GWei   = scala.BigDecimal( BigInt.GWei );
      val Szabo  = scala.BigDecimal( BigInt.Szabo );
      val Finney = scala.BigDecimal( BigInt.Finney );
      val Ether  = scala.BigDecimal( BigInt.Ether );
    }
  }
  trait Multiplier[T] {
    val Wei    : T
    val GWei    : T
    val Szabo  : T
    val Finney : T
    val Ether  : T

    def apply( caseInsensitiveUnitName : String ): T = {
      caseInsensitiveUnitName.toLowerCase match {
        case "wei"    => Wei
        case "gwei"   => GWei
        case "szabo"  => Szabo
        case "finney" => Finney
        case "ether"  => Ether
        case _        => throw new EthereumException( s"Unknown unit: '${caseInsensitiveUnitName}'" )
      }
    }
  }

  private def rounded( bd : BigDecimal ) : BigInt = bd.setScale( 0, BigDecimal.RoundingMode.HALF_UP ).toBigInt

  implicit class DoubleEther ( val value : Double ) extends AnyVal {
    def wei    = rounded( BigDecimal( value ) * Multiplier.BigDecimal.Wei    )
    def gwei   = rounded( BigDecimal( value ) * Multiplier.BigDecimal.GWei   )
    def szabo  = rounded( BigDecimal( value ) * Multiplier.BigDecimal.Szabo  )
    def finney = rounded( BigDecimal( value ) * Multiplier.BigDecimal.Finney )
    def ether  = rounded( BigDecimal( value ) * Multiplier.BigDecimal.Ether  )
  }
  implicit class BigDecimalEther ( val value : BigDecimal ) extends AnyVal {
    def wei    = rounded( value * Multiplier.BigDecimal.Wei    )
    def gwei   = rounded( value * Multiplier.BigDecimal.GWei   )
    def szabo  = rounded( value * Multiplier.BigDecimal.Szabo  )
    def finney = rounded( value * Multiplier.BigDecimal.Finney )
    def ether  = rounded( value * Multiplier.BigDecimal.Ether  )
  }
  implicit class LongEther( val value : Long ) extends AnyVal {
    def wei    = BigInt( value ) * Multiplier.Long.Wei
    def gwei   = BigInt( value ) * Multiplier.Long.GWei
    def szabo  = BigInt( value ) * Multiplier.Long.Szabo
    def finney = BigInt( value ) * Multiplier.Long.Finney
    def ether  = BigInt( value ) * Multiplier.Long.Ether
  }
  implicit class BigIntEther( val value : BigInt ) extends AnyVal {
    def wei    = value * Multiplier.BigInt.Wei
    def gwei   = value * Multiplier.BigInt.GWei
    def szabo  = value * Multiplier.BigInt.Szabo
    def finney = value * Multiplier.BigInt.Finney
    def ether  = value * Multiplier.BigInt.Ether
  }
}
trait Denominations {
  implicit def doubleToDoubleEther( value : Double ) = new Denominations.DoubleEther( value )
  implicit def bigDecimalToBigDecialEther( value : BigDecimal ) = new Denominations.BigDecimalEther( value )
  implicit def longToLongEther( value : Long ) = new Denominations.LongEther( value )
  implicit def bigIntToBigIntEther( value : BigInt ) = new Denominations.BigIntEther( value )
}
