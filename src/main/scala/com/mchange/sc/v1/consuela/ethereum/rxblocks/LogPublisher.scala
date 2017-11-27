package com.mchange.sc.v1.consuela.ethereum.rxblocks

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
class LogPublisher( ethJsonRpcUrl : String, query : Client.LogFilter.Query,  blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : Client.Factory   = Client.Factory.Default,
  scheduler                : Scheduler        = Scheduler.Default,
  executionContext         : ExecutionContext = ExecutionContext.global
) extends SimplePublisher[Client.Log.Recorded,Client.LogFilter]( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )( cfactory, scheduler, executionContext ) {

  import LogPublisher.logger

  val mbLastBlock : Option[BigInt] = {
    val raw = query.toBlock
    raw match {
      case Some( Client.BlockNumber.Quantity( quantity ) ) => Some( quantity )
      case _                                               => None
    }
  }

  protected def acquireFilter( client : Client ) : Future[Client.LogFilter] = client.eth.newLogFilter( query )

  protected def getChanges( client : Client, filter : Client.LogFilter ) : Future[immutable.Seq[Client.Log.Recorded]] = {
    for {
      allLogs <- client.eth.getNewLogs( filter )
    } yield {
      val (recorded, unrecorded) = allLogs.partition( _.isInstanceOf[Client.Log.Recorded] )
      logUnrecorded( unrecorded )
      recorded.map( _.asInstanceOf[Client.Log.Recorded] )
    }
  }

  override protected def transformTerminate( client : Client, items : immutable.Seq[Client.Log.Recorded] ) : Future[Transformed[Client.Log.Recorded]] = {
    mbLastBlock.fold( Future.successful( Transformed(items,false) ) ){ lastBlock =>
      if ( items.nonEmpty ) {
        val shouldTerminate = items.exists( _.blockNumber.widen > lastBlock )
        val preterminal = if ( shouldTerminate ) {
          items.takeWhile( _.blockNumber.widen <= lastBlock )
        } else {
          items
        }
        Future.successful( Transformed( preterminal, shouldTerminate ) )
      }
      else { // check the current block number to see if we can terminate
        client.eth.blockNumber() map { curBlock =>
          Transformed( items, curBlock > lastBlock )
        }
      }
    }
  }

  private def logUnrecorded( unrecorded : immutable.Seq[Client.Log] ) : Unit = {
    unrecorded.foreach( clog => WARNING.log( s"A removed or pending log was found by ${this}, and was not published: ${clog}." ) )
  }
}

