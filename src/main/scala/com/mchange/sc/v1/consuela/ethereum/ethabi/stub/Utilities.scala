package com.mchange.sc.v1.consuela.ethereum.ethabi.stub

import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela.ethereum.EthHash
import com.mchange.sc.v1.consuela.ethereum.specification.Denominations
import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.{ClientTransactionReceipt,Invoker}

trait Utilities extends Denominations {
  def futureTransactionReceipt( transactionHash : EthHash, timeout : Duration )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[Option[ClientTransactionReceipt]] = {
    Invoker.futureTransactionReceipt( transactionHash, timeout )
  }
  def awaitTransactionReceipt( transactionHash : EthHash, timeout : Duration )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Option[ClientTransactionReceipt] = {
    Invoker.awaitTransactionReceipt( transactionHash, timeout )
  }
  def requireTransactionReceipt( transactionHash : EthHash, timeout : Duration = Duration.Inf )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : ClientTransactionReceipt = {
    Invoker.requireTransactionReceipt( transactionHash, timeout )
  }
}
