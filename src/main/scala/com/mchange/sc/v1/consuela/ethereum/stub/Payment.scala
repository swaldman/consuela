package com.mchange.sc.v1.consuela.ethereum.stub

object Payment {
  final case class ofWei( amountInWei : sol.UInt256 ) extends Payment
  final case object None extends Payment {
    val amountInWei : sol.UInt256 = Zero256
  }
}
trait Payment {
  def amountInWei : sol.UInt256
}

