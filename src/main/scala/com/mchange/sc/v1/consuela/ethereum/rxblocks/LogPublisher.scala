package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v2.jsonrpc.Exchanger

import com.mchange.sc.v1.consuela.ethereum.EthHash
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

import com.mchange.sc.v2.concurrent.Scheduler

import com.mchange.sc.v1.log.MLevel._

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import SimplePublisher.Transformed

object LogPublisher {
  private [LogPublisher] implicit lazy val logger = mlogger( this )
}
class LogPublisher( ethJsonRpcUrl : String, query : Client.Log.Filter.Query,  blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  efactory                 : Exchanger.Factory = Exchanger.Factory.Default,
  scheduler                : Scheduler         = Scheduler.Default,
  executionContext         : ExecutionContext  = ExecutionContext.global
) extends SimplePublisher[Client.Log,Client.Log,Client.Log.Filter]( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )( efactory, scheduler, executionContext ) {

  import LogPublisher.logger

  val mbEndBlock : Option[BigInt] = {
    val raw = query.toBlock
    raw match {
      case Some( Client.BlockNumber.Quantity( quantity ) ) => Some( quantity )
      case _                                               => None
    }
  }

  protected def acquireFilter( client : Client ) : Future[Client.Log.Filter] = client.eth.newLogFilter( query )

  protected def getChanges( client : Client, filter : Client.Log.Filter ) : Future[immutable.Seq[Client.Log]] = {
    client.eth.getNewLogs( filter )
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

