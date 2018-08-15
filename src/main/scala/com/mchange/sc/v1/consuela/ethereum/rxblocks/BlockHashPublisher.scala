package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v2.jsonrpc.Exchanger

import com.mchange.sc.v1.consuela.ethereum.EthHash
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client
import Client.BlockFilter

import com.mchange.sc.v2.concurrent.Scheduler

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import SimplePublisher.Transformed

class BlockHashPublisher( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  efactory                 : Exchanger.Factory = Exchanger.Factory.Default,
  scheduler                : Scheduler         = Scheduler.Default,
  executionContext         : ExecutionContext  = ExecutionContext.global
) extends SimplePublisher[EthHash,EthHash,BlockFilter]( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )( efactory, scheduler, executionContext ) {
  protected def acquireFilter( client : Client )                    : Future[BlockFilter]            = client.eth.newBlockFilter()
  protected def getChanges( client : Client, filter : BlockFilter ) : Future[immutable.Seq[EthHash]] = client.eth.getBlockFilterChanges(filter)

  protected def transformTerminate( client : Client, items : immutable.Seq[EthHash] ) : Future[Transformed[EthHash]] = {
    Future.successful( Transformed[EthHash]( items, false ) )
  }
}
