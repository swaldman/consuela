package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v2.jsonrpc.Exchanger

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client
import Client.Filter

import com.mchange.sc.v2.concurrent.Scheduler

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import NoFilterPublisher.Transformed

class BlockNumberNoFilterPublisher( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  efactory                 : Exchanger.Factory = Exchanger.Factory.Default,
  scheduler                : Scheduler         = Scheduler.Default,
  executionContext         : ExecutionContext  = ExecutionContext.global
) extends NoFilterPublisher[BigInt,BigInt]( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )( efactory, scheduler, executionContext ) {

  //MT: protected by this' lock
  private var lastSeen : BigInt = BigInt(-1)

  protected def getChanges( client : Client, fromBlock : BigInt, toBlock : BigInt ) : Future[immutable.Seq[BigInt]] = {
    for {
      current <- client.eth.blockNumber()
    } yield {
      this.synchronized {
        if ( current > lastSeen ) {
          val out = {
            if ( lastSeen >= 0 ) { // don't publish every blocknumber from genesis to the first we see
              (lastSeen + 1) to current
            } else {
              current :: Nil
            }
          }
          lastSeen = current
          out
        }
        else {
          Nil
        }
      }
    }
  }

  protected def transformTerminate( client : Client, items : immutable.Seq[BigInt] ) : Future[Transformed[BigInt]] = {
    Future.successful( Transformed[BigInt]( items, false ) )
  }
}
