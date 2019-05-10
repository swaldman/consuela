package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v2.jsonrpc.Exchanger

import com.mchange.sc.v1.consuela.ethereum.EthHash
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

import com.mchange.sc.v2.concurrent.Scheduler

import com.mchange.sc.v1.log.MLevel._

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import NoFilterPublisher.Transformed

object LogNoFilterPublisher {
  private [LogNoFilterPublisher] implicit lazy val logger = mlogger( this )
}
class LogNoFilterPublisher( ethJsonRpcUrl : String, query : Client.Log.Filter.Query,  blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  efactory                 : Exchanger.Factory = Exchanger.Factory.Default,
  scheduler                : Scheduler         = Scheduler.Default,
  executionContext         : ExecutionContext  = ExecutionContext.global
) extends NoFilterPublisher[Client.Log,Client.Log]( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )( efactory, scheduler, executionContext ) {

  import LogNoFilterPublisher.logger

  val mbEndBlock : Option[BigInt] = {
    val raw = query.toBlock
    raw match {
      case Some( Client.BlockNumber.Quantity( quantity ) ) => Some( quantity )
      case _                                               => None
    }
  }

  protected def getChanges( client : Client, fromBlock : BigInt, toBlock : BigInt ) : Future[immutable.Seq[Client.Log]] = {
    assert( fromBlock <= toBlock, s"fromBlock must be less than or equal to toBlock, isn't -- fromBlock: ${fromBlock}, toBlock: ${toBlock}" )

    mbEndBlock match {
      case Some( maxValue ) => {
        if (fromBlock > maxValue ) {
          Future.successful( immutable.Seq.empty )
        }
        else {
          val currentQuery = query.copy( fromBlock=Some( Client.BlockNumber.Quantity( fromBlock ) ), toBlock=Some( Client.BlockNumber.Quantity( toBlock.min(maxValue) ) ) )
          client.eth.getLogs( currentQuery )
        }
      }
      case None => {
        val currentQuery = query.copy( fromBlock=Some( Client.BlockNumber.Quantity( fromBlock ) ), toBlock=Some( Client.BlockNumber.Quantity( toBlock ) ) )
        client.eth.getLogs( currentQuery )
      }
    }
  }

  override protected def transformTerminate( client : Client, items : immutable.Seq[Client.Log] ) : Future[Transformed[Client.Log]] = {
    mbEndBlock.fold( Future.successful( Transformed(items,false) ) ){ endBlock =>
      val ( rawFull, pending ) = items.partition( _.isInstanceOf[Client.Log.Full] )
      val full = rawFull.map( _.asInstanceOf[Client.Log.Full] )
      if ( full.nonEmpty ) {
        def expired( clf : Client.Log.Full ) : Boolean = clf.blockNumber.widen > endBlock

        val shouldTerminate = full.exists( expired )
        val preterminal = if ( shouldTerminate ) {
          val bad = full.filter( expired )
          items.filterNot( bad.contains(_) )
        } else {
          items
        }
        Future.successful( Transformed( preterminal, shouldTerminate ) )
      }
      else { // check the current block number to see if we can terminate
        client.eth.blockNumber() map { curBlock =>
          Transformed( items, curBlock > endBlock )
        }
      }
    }
  }
}

