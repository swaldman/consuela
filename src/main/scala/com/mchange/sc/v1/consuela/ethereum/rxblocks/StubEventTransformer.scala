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

object StubEventTransformer {
  implicit lazy val logger = mlogger( this )
}
final class StubEventTransformer( abi : Abi, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  scheduler : Scheduler,
  executionContext : ExecutionContext
) extends RxProcessor[Client.Log.Recorded,(SolidityEvent, stub.Event.Metadata)] with SimpleSubscription.Parent[(SolidityEvent, stub.Event.Metadata)] {
  import StubEventTransformer.logger

  val interpretor = SolidityEvent.Interpretor( abi )

  // MT: protected by this' lock
  private var recordedSubscription : RxSubscription = null
  private val subscriptions : mutable.HashSet[SimpleSubscription[(SolidityEvent, stub.Event.Metadata)]] = mutable.HashSet.empty[SimpleSubscription[(SolidityEvent, stub.Event.Metadata)]]

  private def addSubscription( s : SimpleSubscription[(SolidityEvent, stub.Event.Metadata)] ) : Unit = this.synchronized {
    subscriptions += s
  }
  private [rxblocks] def removeSubscription( s : SimpleSubscription[(SolidityEvent, stub.Event.Metadata)] ) : Unit = this.synchronized {
    subscriptions -= s
  }
  private def init( rs : RxSubscription ) : Unit = this.synchronized {
    recordedSubscription = rs
  }
  private def broadcast( terminate : Boolean )( f : (SimpleSubscription[(SolidityEvent, stub.Event.Metadata)]) => Unit ) : Unit = this.synchronized {
    subscriptions.foreach( f )
    if ( terminate ) recordedSubscription = null
  }

  def ftransform( recorded : Client.Log.Recorded ) : Failable[ (SolidityEvent, stub.Event.Metadata) ] = {
    for {
      solidityEvent <- interpretor.interpret( recorded.ethLogEntry )
    }
    yield {
      ( solidityEvent, stub.Event.Metadata( recorded ) )
    }
  }

  def onSubscribe( subscription : RxSubscription ) : Unit = init( subscription )
  def onComplete()                                 : Unit = broadcast( terminate = true )( _.complete() )
  def onError( t : Throwable )                     : Unit = broadcast( terminate = true )( _.break(t) )
  def onNext( recorded : Client.Log.Recorded )     : Unit = {
    try {
      val pair = ftransform( recorded ).get
      broadcast( terminate = false )( _.enqueue( List( pair ), false ) )
    }
    catch {
      case t : Throwable => broadcast( terminate = true )( _.break(t) )
    }
  }

  def subscribe( s : RxSubscriber[_ >: ( SolidityEvent, stub.Event.Metadata ) ] ) = {
    val subscription = new SimpleSubscription[(SolidityEvent, stub.Event.Metadata)]( this, s, subscriptionUpdateDelay )
    addSubscription( subscription )
    s.onSubscribe( subscription )
  }
}
