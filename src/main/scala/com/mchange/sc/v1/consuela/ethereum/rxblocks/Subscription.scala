package com.mchange.sc.v1.consuela.ethereum.rxblocks

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import org.reactivestreams.{Publisher => RxPublisher, Subscriber => RxSubscriber, Subscription => RxSubscription}

import com.mchange.sc.v1.log.MLevel._
import com.mchange.sc.v1.log.MLogger

import com.mchange.sc.v2.concurrent.Scheduler

object SimpleSubscription {
  trait Parent[T] {
    private [rxblocks] def removeSubscription( subscription : SimpleSubscription[T] ) : Unit
  }
}
class SimpleSubscription[T] private [rxblocks] (
  parent : SimpleSubscription.Parent[T],
  subscriber : RxSubscriber[_ >: T],
  subscriptionUpdateDelay : Duration = 3.seconds
)(implicit logger : MLogger, scheduler : Scheduler, executionContext : ExecutionContext ) extends RxSubscription {

  // MT: Protected by this' lock
  private var active = true
  private var terminated = false // this means we still have buffered items to signal, but we've received a logical termination and will accept no more
  private var requested : Long = 0
  private val q = mutable.Queue.empty[T]

  // MT: should be called only by blocks synchronized on this' lock
  private def readyToPublish = requested > 0 && q.nonEmpty
  private def readyToComplete = q.isEmpty && terminated

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
      case t : Throwable => this.break( t )
    }
  }

  private def rescheduleUpdate() : Unit = {
    scheduler.schedule( updateSubscriber _, subscriptionUpdateDelay )
  }

  private [rxblocks] def break( t : Throwable ) : Unit = {
    subscriber.onError( t )
    this.cancel()
  }

  private [rxblocks] def enqueue( nexts : immutable.Seq[T], shouldTerminate : Boolean ) = this.synchronized {
    if ( active ) {
      if (! terminated ) { // not previously terminated!
        nexts.foreach( q.enqueue( _ ) )
        if ( shouldTerminate ) {
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
    parent.removeSubscription( this )
  }
}
