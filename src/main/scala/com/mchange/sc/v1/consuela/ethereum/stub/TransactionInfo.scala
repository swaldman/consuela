package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection.immutable
import com.mchange.sc.v1.consuela.ethereum.{EthHash,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.ClientTransactionReceipt


object TransactionInfo {
  final object Details {
    case class Message (
      transactionIndex  : sol.UInt256,
      blockHash         : EthHash,
      blockNumber       : sol.UInt256,
      cumulativeGasUsed : sol.UInt256,
      gasUsed           : sol.UInt256,
      logs              : immutable.Seq[EthLogEntry]
    ) extends Details
    case class ContractDeployment(
      contractAddress   : sol.Address,
      transactionIndex  : sol.UInt256,
      blockHash         : EthHash,
      blockNumber       : sol.UInt256,
      cumulativeGasUsed : sol.UInt256,
      gasUsed           : sol.UInt256,
      logs              : immutable.Seq[EthLogEntry]
    ) extends Details
  }
  trait Details {
    def transactionIndex  : sol.UInt256
    def blockHash         : EthHash
    def blockNumber       : sol.UInt256
    def cumulativeGasUsed : sol.UInt256
    def gasUsed           : sol.UInt256
    def logs              : immutable.Seq[EthLogEntry]
  }

  object Message {
    def fromJsonrpcReceipt( transactionHash : EthHash, mbReceipt : Option[ClientTransactionReceipt] ) : TransactionInfo.Message = {
      val details = mbReceipt.map( ctr => TransactionInfo.Details.Message( ctr.transactionIndex, ctr.blockHash, ctr.blockNumber, ctr.cumulativeGasUsed, ctr.gasUsed, ctr.logs ) )
      this.apply( transactionHash, details )
    }
  }
  case class Message (
    val transactionHash : EthHash,
    val details         : Option[TransactionInfo.Details.Message]
  ) extends TransactionInfo

  case class ContractDeployment (
    val transactionHash : EthHash,
    val details         : Option[TransactionInfo.Details.ContractDeployment]
  ) extends TransactionInfo
}
trait TransactionInfo {
  def transactionHash : EthHash
  def details : Option[TransactionInfo.Details]
}
