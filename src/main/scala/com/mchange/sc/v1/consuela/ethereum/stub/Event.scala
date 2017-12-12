package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.{jsonrpc,EthHash,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256
import jsonrpc.Client

object Event {
  final object Metadata {
    def apply( recorded : Client.Log.Recorded ) : Metadata = {
      this.apply (
        logIndex         = recorded.logIndex,
        transactionIndex = recorded.transactionIndex,
        transactionHash  = recorded.transactionHash,
        blockHash        = recorded.blockHash,
        blockNumber      = recorded.blockNumber
      )
    }
  }
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
