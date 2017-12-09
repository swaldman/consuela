package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.{EthHash,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256

object Event {
  final case class Metadata (
    val logIndex         : Unsigned256,
    val transactionIndex : Unsigned256,
    val transactionHash  : EthHash,
    val blockHash        : EthHash,
    val blockNumber      : Unsigned256
  )
}
trait Event {
  def metadata : Event.Metadata
  def logEntry : EthLogEntry
}
