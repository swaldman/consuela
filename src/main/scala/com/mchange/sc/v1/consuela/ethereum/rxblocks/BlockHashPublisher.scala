package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.log.MLevel._
import com.mchange.sc.v1.consuela.ethereum.{jsonrpc,EthHash}
import com.mchange.sc.v2.concurrent.Scheduler
import com.mchange.sc.v2.failable._
import com.mchange.sc.v2.lang._

import org.reactivestreams.{Publisher, Subscriber, Subscription => RxSubscription}

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._


// maybe we want a constructor arg (mbStartNum : Option[BigInt]), so we can begin
// at an arbitrary block. But that introduces lots of complexity. Let's keep this
// simple for now

object BlockHashPublisher {
  private [rxblocks] implicit lazy val logger = mlogger( this )
}
class BlockHashPublisher( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,
  scheduler                : Scheduler              = Scheduler.Default,
  executionContext         : ExecutionContext       = ExecutionContext.global
) extends Publisher[EthHash] {

  import BlockHashPublisher.logger

  def subscribe( subscriber : Subscriber[_ >: EthHash] ) : Unit = {
    val subscription = this.synchronized {
      val s = new Subscription( subscriber )
      subscriptions += s

      // startup polling, enqueuing, dequeuing
      if (scheduled == null) startupPolling()

      s
    }
    subscriber.onSubscribe( subscription )
  }

  //MT: Protected by this' lock
  private val subscriptions : mutable.Set[Subscription]          = mutable.HashSet.empty[Subscription]
  private var scheduled     : Scheduler.Scheduled[Unit]          = null
  private var client        : jsonrpc.Client                     = null
  private var f_filter      : Future[jsonrpc.Client.BlockFilter] = null

  private def removeSubscription( subscription : Subscription ) = this.synchronized {
    subscriptions -= subscription
    if ( subscriptions.isEmpty ) shutdownPolling()
  }

  // call only while holding this' lock
  private def startupPolling() = {
    try {
      this.client = cfactory( ethJsonRpcUrl )
      this.f_filter = client.eth.newBlockFilter()
      this.scheduled = scheduler.scheduleWithFixedDelay( doPoll _, 0.seconds, blockPollDelay )
    }
    catch {
      case t : Throwable => {
        shutdownPolling()
        throw t
      }
    }
  }

  private def doPoll() = {
    val ( c, ff, sl ) = this.synchronized { (client, f_filter, subscriptions.toList) }
    for {
      filter <- ff
      hashes <- c.eth.getBlockFilterChanges(filter)
    } {
       sl.foreach( _.enqueue( hashes ) )
    }
  }

  private def shutdownPolling() = this.synchronized {
    if ( scheduled != null ) {
      Failable( scheduled.attemptCancel() ).xwarn("Exception while canceling polling")
    }
    if ( this.client != null ) {
      Failable {
        try {
          if ( this.f_filter != null) {
            f_filter flatMap ( client.eth.uninstallFilter( _ ) )
          }
        }
        finally {
          attemptClose( this.client )
        }
      }.xwarn("Exception while uninstalling filter")
    }
    this.scheduled = null
    this.client = null
    this.f_filter = null
  }

  class Subscription( subscriber : Subscriber[_ >: EthHash] ) extends RxSubscription {

    //MT: Protected by this' lock
    private var active = true
    private var requested : Long = 0
    private val q = mutable.Queue.empty[EthHash]

    private def update() : Unit = {
      try {
        this.synchronized {
          while ( requested > 0 && q.nonEmpty ) {
            subscriber.onNext( q.dequeue )
            requested -= 1
          }
        }
      }
      catch {
        case t : Throwable => {
          subscriber.onError( t )
          this.cancel()
        }
      }
    }

    private val scheduled = scheduler.scheduleWithFixedDelay( update _, 0.seconds, subscriptionUpdateDelay )

    private [BlockHashPublisher] def enqueue( next : EthHash ) = this.synchronized {
      if ( active ) q.enqueue( next )
    }

    private [BlockHashPublisher] def enqueue( nexts : Seq[EthHash] ) = this.synchronized {
      if ( active ) nexts.foreach( q.enqueue( _ ) )
    }

    def request( n : Long ) : Unit = this.synchronized {
      require( n >= 0, s"Only positive quantities should be requested from a publisher. Requested ${n}." )
      requested = {
        if ( (Long.MaxValue - n) < requested ) {
          Long.MaxValue
        } else {
          requested + n
        }
      }
    }

    def cancel() = {
      try {
        this.synchronized {
          this.active = false
        }
        removeSubscription( this )
      }
      finally {
        scheduled.attemptCancel()
      }
    }
  }
}
