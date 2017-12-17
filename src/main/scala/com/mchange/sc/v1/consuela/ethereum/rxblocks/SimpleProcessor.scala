package com.mchange.sc.v1.consuela.ethereum.rxblocks

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Abi,Client}
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent
import com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v2.concurrent.Scheduler

import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.log.MLevel._

import org.reactivestreams.{Publisher => RxPublisher, Subscriber => RxSubscriber, Subscription => RxSubscription, Processor => RxProcessor}

object SimpleProcessor {
  implicit lazy val logger = mlogger( this )
}
abstract class SimpleProcessor[FROM,TO]( subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  scheduler : Scheduler,
  executionContext : ExecutionContext
) extends RxProcessor[FROM,TO] with SimpleSubscription.Parent[TO] {
  import SimpleProcessor.logger

  def ftransform( recorded : FROM ) : Failable[TO]

  // MT: protected by this' lock
  private var sourceSubscription : RxSubscription = null
  private val subscriptions : mutable.HashSet[SimpleSubscription[TO]] = mutable.HashSet.empty[SimpleSubscription[TO]]
  private var requested : Long = 0
  private var requestUpdaterScheduled : Scheduler.Scheduled[Unit] = null

  //MT: call only from methods holding this' lock
  private def ensureRequestUpdaterState() = {
    if ( subscriptions.isEmpty ) {
      if ( requestUpdaterScheduled != null ) {
        requestUpdaterScheduled.attemptCancel()
        requestUpdaterScheduled = null
      }
    }
    else {
      if ( requestUpdaterScheduled == null ) {
        // println( s"Starting ${this}.updateRequests cycle" )
        requestUpdaterScheduled = scheduler.scheduleAtFixedRate( () => updateRequests(), 0.seconds, subscriptionUpdateDelay ) // this line starts the processor requesting
      }
    }
  }

  private def addSubscription( s : SimpleSubscription[TO] ) : Unit = this.synchronized {
    subscriptions += s
    ensureRequestUpdaterState()
  }
  private [rxblocks] def removeSubscription( s : SimpleSubscription[TO] ) : Unit = this.synchronized {
    subscriptions -= s
    ensureRequestUpdaterState()
  }
  private def init( rs : RxSubscription ) : Unit = this.synchronized {
    sourceSubscription = rs
  }
  private def broadcast( terminate : Boolean, decrement : Long = 0 )( f : (SimpleSubscription[TO]) => Unit ) : Unit = this.synchronized {
    subscriptions.foreach( f )
    requested -= decrement
    if ( terminate && sourceSubscription != null ) {
      sourceSubscription.cancel()
      sourceSubscription = null
    }
  }

  private def updateRequests() : Unit = this.synchronized {
    try {
      // println( s"${this}.updateRequest() [beginning]" )
      val needed = subscriptions.map( _.requestedCount ).max
      val increment = needed - this.requested
      if ( increment > 0 ) {
        if ( Long.MaxValue - increment >= requested ) {
          request( increment )
        }
        else {
          request( Long.MaxValue - requested )
        }
      }
      // println( s"${this}.updateRequest() [needed=$needed; increment=$increment]" )
    }
    catch {
      case t : Throwable => {
        t.printStackTrace()
        throw t
      }
    }
  }

  // MT: Must be called only from methods holding this' lock
  private def request( n : Long ) : Unit = {
    if ( sourceSubscription != null ) {
      if ( n == Long.MaxValue ) {
        sourceSubscription.request( Long.MaxValue )
      }
      else {
        sourceSubscription.request( n )
        this.requested += n
      }
    }
  }

  def onSubscribe( subscription : RxSubscription ) : Unit = init( subscription )
  def onComplete()                                 : Unit = broadcast( terminate = true )( _.complete() ) // cancels our subscribers
  def onError( t : Throwable )                     : Unit = broadcast( terminate = true )( _.break(t) ) // cancels our subscribers
  def onNext( recorded : FROM )                    : Unit = {
    try {
      val pair = ftransform( recorded ).get
      broadcast( terminate = false, decrement = 1L )( _.enqueue( List( pair ), false ) )
    }
    catch {
      case t : Throwable => {
        broadcast( terminate = true )( _.break(t) ) // cancels our subscribers
      }
    }
  }

  def subscribe( s : RxSubscriber[_ >: TO] ) = {
    val subscription = new SimpleSubscription[TO]( this, s, subscriptionUpdateDelay )
    addSubscription( subscription )
    s.onSubscribe( subscription )
  }
}
