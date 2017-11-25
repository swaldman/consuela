package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client
import Client.Filter

import com.mchange.sc.v2.concurrent.Scheduler
import com.mchange.sc.v2.failable._
import com.mchange.sc.v2.lang._

import org.reactivestreams.{Publisher, Subscriber, Subscription => RxSubscription}

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._


/**
  * Unless the chosen filter supports a "fromBlock", there is no way to control precisely at what
  * block a subscription begins. A subscriber receives events "as they happen", and even if
  * they subscribe very quickly after e.g. contract deployment, there can be no guarantee that
  * they won't miss anything.
  * 
  * Fortunately, the most important filters (log filters) do support "fromBlock"
  */
object SimplePublisher {
  private [rxblocks] implicit lazy val logger = mlogger( this )
}
abstract class SimplePublisher[T, F <: Filter]( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : Client.Factory   = Client.Factory.Default,
  scheduler                : Scheduler        = Scheduler.Default,
  executionContext         : ExecutionContext = ExecutionContext.global // XXX: SHould we reorganize so that by default we use the Scheduler's thread pool?
) extends Publisher[T] {

  import SimplePublisher.logger

  protected def acquireFilter( client : Client )          : Future[F]
  protected def getChanges( client : Client, filter : F ) : Future[immutable.Seq[T]]

  protected def isTerminating( items : immutable.Seq[T] ) : Boolean = false

  protected def cancelFilter( client : Client, filter : F ) : Future[_] = {
    client.eth.uninstallFilter( filter )
  }

  def subscribe( subscriber : Subscriber[_ >: T] ) : Unit = {
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
  private val subscriptions : mutable.Set[Subscription] = mutable.HashSet.empty[Subscription]
  private var scheduled     : Scheduler.Scheduled[Unit] = null
  private var _client       : Client                    = null
  private var f_filter      : Future[F]                 = null

  private def removeSubscription( subscription : Subscription ) = this.synchronized {
    subscriptions -= subscription
    if ( subscriptions.isEmpty ) shutdownPolling()
  }

  // call only while holding this' lock
  private def startupPolling() = {
    try {
      this._client = cfactory( ethJsonRpcUrl )
      this.f_filter = acquireFilter( this._client )
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
    val ( c, ff, sl ) = this.synchronized { (_client, f_filter, subscriptions.toList) }
    for {
      filter <- ff
      items <- getChanges( this._client, filter )
    } {
       sl.foreach( _.enqueue( items ) )
    }
  }

  private def shutdownPolling() = this.synchronized {
    if ( scheduled != null ) {
      Failable( scheduled.attemptCancel() ).xwarn("Exception while canceling polling")
    }
    if ( this._client != null ) {
      Failable {
        try {
          if ( this.f_filter != null) {
            f_filter flatMap ( filter => cancelFilter( this._client, filter ) )
          }
        }
        finally {
          attemptClose( this._client )
        }
      }.xwarn("Exception while uninstalling filter")
    }
    this.scheduled = null
    this._client = null
    this.f_filter = null
  }

  class Subscription( subscriber : Subscriber[_ >: T] ) extends RxSubscription {

    // MT: Protected by this' lock
    private var active = true
    private var terminated = false // this means we still have buffered items to signal, but we've received a logical termination and will accept no more
    private var requested : Long = 0
    private val q = mutable.Queue.empty[T]

    // MT: should be called only by blocks synchronized on this' lock
    def readyToPublish = requested > 0 && q.nonEmpty
    def readyToComplete = q.isEmpty && terminated

    // start the update process...
    Future( updateSubscriber() )

    // note that we take care not to interact with subscribers while holding this' lock
    // as this would permit a bad subscriber from holding up the publisher's attempts to
    // enqueue
    //
    // we don't use a repeating schedule, so there is no risk that a slow subscriber could
    // cause updates to pile up and be concurrent to the subscriber and/or out of order

    private def updateSubscriber() : Unit = {
      try {
        val tmpQ = mutable.Queue.empty[T] // local to this methods, so we can interact without syncing
        this.synchronized {
          while ( active && readyToPublish ) {
            tmpQ.enqueue( q.dequeue )
            requested -= 1
          }
        }
        tmpQ.foreach( subscriber.onNext( _ ) )
        val shouldComplete = this.synchronized {
          active && readyToComplete
        }
        if ( shouldComplete ) {
          subscriber.onComplete()
          this.cancel()
        }
        val again = this.synchronized { active }
        if ( again ) rescheduleUpdate()
      }
      catch {
        case t : Throwable => {
          subscriber.onError( t )
          this.cancel()
        }
      }
    }

    private def rescheduleUpdate() : Unit = {
      scheduler.schedule( updateSubscriber _, subscriptionUpdateDelay )
    }

    private [SimplePublisher] def enqueue( nexts : immutable.Seq[T] ) = this.synchronized {
      if ( active ) {
        if (! terminated ) { // not previously terminated!
          nexts.foreach( q.enqueue( _ ) )
          if ( isTerminating( nexts ) ) {
            this.terminated = true
          }
        }
        else {
          WARNING.log( "Received items after this publisher has been marked terminated. This is probably a bug in the definition of the publisher! Ignoring items." )
        }
      }
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
      this.synchronized {
        this.active = false
      }
      SimplePublisher.this.removeSubscription( this )
    }
  }
}
