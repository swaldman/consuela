package com.mchange.sc.v1.consuela.ethereum.stub

object Nonce {
  def apply( mbValue : Option[sol.UInt256] ) : Nonce = {
    mbValue match {
      case Some( value ) => Nonce.withValue( value )
      case None          => Nonce.Auto
    }
  }
  def apply( value : BigInt )      : Nonce = Nonce.withValue( sol.UInt256( value ) )
  def apply( value : Long )        : Nonce = Nonce.withValue( sol.UInt256( value ) )
  def apply( value : Int )         : Nonce = Nonce.withValue( sol.UInt256( value ) )

  final case class withValue( value : sol.UInt256 ) extends Nonce {
    def toOption : Option[sol.UInt256] = Some( value )
  }
  final case object Auto extends Nonce {
    val toOption : Option[sol.UInt256] = None
  }
}
trait Nonce {
  def toOption : Option[sol.UInt256]
}

