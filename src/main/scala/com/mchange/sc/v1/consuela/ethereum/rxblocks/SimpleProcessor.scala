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

object SimpleTransformer {
  implicit lazy val logger = mlogger( this )
}
abstract class SimpleTransformer[FROM,TO]( subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  scheduler : Scheduler,
  executionContext : ExecutionContext
) extends RxProcessor[FROM,TO] with SimpleSubscription.Parent[TO] {
  import SimpleTransformer.logger

  def ftransform( recorded : FROM ) : Failable[TO]

  // MT: protected by this' lock
  private var recordedSubscription : RxSubscription = null
  private val subscriptions : mutable.HashSet[SimpleSubscription[TO]] = mutable.HashSet.empty[SimpleSubscription[TO]]

  private def addSubscription( s : SimpleSubscription[TO] ) : Unit = this.synchronized {
    subscriptions += s
  }
  private [rxblocks] def removeSubscription( s : SimpleSubscription[TO] ) : Unit = this.synchronized {
    subscriptions -= s
  }
  private def init( rs : RxSubscription ) : Unit = this.synchronized {
    recordedSubscription = rs
  }
  private def broadcast( terminate : Boolean )( f : (SimpleSubscription[TO]) => Unit ) : Unit = this.synchronized {
    subscriptions.foreach( f )
    if ( terminate ) recordedSubscription = null
  }

  def onSubscribe( subscription : RxSubscription ) : Unit = init( subscription )
  def onComplete()                                 : Unit = broadcast( terminate = true )( _.complete() )
  def onError( t : Throwable )                     : Unit = broadcast( terminate = true )( _.break(t) )
  def onNext( recorded : FROM )                    : Unit = {
    try {
      val pair = ftransform( recorded ).get
      broadcast( terminate = false )( _.enqueue( List( pair ), false ) )
    }
    catch {
      case t : Throwable => broadcast( terminate = true )( _.break(t) )
    }
  }

  def subscribe( s : RxSubscriber[_ >: TO] ) = {
    val subscription = new SimpleSubscription[TO]( this, s, subscriptionUpdateDelay )
    addSubscription( subscription )
    s.onSubscribe( subscription )
  }
}
