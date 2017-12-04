package com.mchange.sc.v1.consuela.ethereum.rxblocks

import scala.collection._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import com.mchange.sc.v1.log.MLevel._

import org.reactivestreams.{Publisher => RxPublisher, Subscriber => RxSubscriber, Subscription => RxSubscription}

import com.mchange.sc.v1.consuela.ethereum.{EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

import com.mchange.sc.v2.concurrent.Scheduler

object ConfirmedLogPublisher {
  private lazy implicit val logger = mlogger(this)
}
class ConfirmedLogPublisher( ethJsonRpcUrl : String, query : Client.Log.Filter.Query, numConfirmations : Int, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : Client.Factory   = Client.Factory.Default,
  scheduler                : Scheduler        = Scheduler.Default,
  executionContext         : ExecutionContext = ExecutionContext.global
) extends RxPublisher[Client.Log.Recorded] with SimpleSubscription.Parent[Client.Log.Recorded] {

  import ConfirmedLogPublisher._

  private val logPublisher = new LogPublisher( ethJsonRpcUrl, query, blockPollDelay, subscriptionUpdateDelay )
  private val numPublisher = new BlockNumberPublisher( ethJsonRpcUrl, blockPollDelay, subscriptionUpdateDelay )

  //MT: protected by this' lock
  private val pendingConfirmation = createPendingConfirmations 
  private val subscriptions       = mutable.HashSet.empty[SimpleSubscription[Client.Log.Recorded]]

  //MT: protected by this' lock
  private var logSubscription : RxSubscription = null
  private var numSubscription : RxSubscription = null

  //MT: Use in constructor only
  private def createPendingConfirmations() = { // grrr... mutable TreeMaps weren't implemented until Scala 2.12
    import scala.collection.JavaConverters._

    val jmap = new java.util.TreeMap[BigInt,mutable.HashMap[EthLogEntry,Client.Log.Recorded]]
    jmap.asScala
  }

  def subscribe( subscriber : RxSubscriber[_ >: Client.Log.Recorded] ) : Unit = {
    val subscription = this.synchronized {
      val s = new SimpleSubscription[Client.Log.Recorded]( this, subscriber )
      subscriptions += s

      // startup polling, enqueuing, dequeuing
      if (logSubscription == null) initSubscriptions()

      s
    }
    subscriber.onSubscribe( subscription )
  }

  private [rxblocks] def removeSubscription( subscription : SimpleSubscription[Client.Log.Recorded] ) = this.synchronized {
    subscriptions -= subscription
    if ( subscriptions.isEmpty ) cancelSubscriptions()
  }

  private [rxblocks] def removeAllSubscriptions() = this.synchronized {
    subscriptions.clear()
    cancelSubscriptions()
  }

  private def initSubscriptions() : Unit = {
    logPublisher.subscribe( LogSubscriber )
    numPublisher.subscribe( BlockNumSubscriber )
  }

  //MT: Must be called from within a this-synchronized block
  private def cancelSubscriptions() : Unit = {
    if ( logSubscription != null) {
      logSubscription.cancel()
      logSubscription = null
    }
    if ( numSubscription != null) {
      numSubscription.cancel()
      numSubscription = null
    }
  }

  private def newLog( log : Client.Log ) : Unit = {
    log match {
      case recorded : Client.Log.Recorded => addPending( recorded )
      case removed  : Client.Log.Removed  => removePending( removed )
      case pending  : Client.Log.Pending  => {
        DEBUG.log( s"${this} saw a pending log. Ignored. Log -> ${pending}" )
      }
    }
  }

  private def addPending( recorded : Client.Log.Recorded ) : Unit = this.synchronized {
    val mbBlockMap = pendingConfirmation.get( recorded.blockNumber.widen )
    val logMap = {
      mbBlockMap match {
        case Some( map ) => map
        case None => {
          val out =  mutable.HashMap.empty[EthLogEntry,Client.Log.Recorded]
          pendingConfirmation.put( recorded.blockNumber.widen, out )
          out
        }
      }
    }
    logMap += Tuple2( recorded.ethLogEntry, recorded )
  }

  private def removePending( removed : Client.Log.Removed ) : Unit = this.synchronized {
    val mbBlockMap = pendingConfirmation.get( removed.blockNumber.widen )
    mbBlockMap.foreach { blockMap =>
      blockMap -= removed.ethLogEntry
      if ( blockMap.isEmpty ) {
        pendingConfirmation -= removed.blockNumber.widen
      }
    }
  }

  private def newBlockNum( blockNum : BigInt ) : Unit = {
    val (confirmed, subs) = this.synchronized { // immutable snapshot of subscriptions, so we don't hold this' lock when we enqueue
      val tmpConfirmed = pendingConfirmation takeWhile { case ( lbn, map ) =>
        lbn <= (blockNum - numConfirmations)
      }
      pendingConfirmation --= tmpConfirmed.keys
      ( tmpConfirmed, subscriptions )
    }
    val logOrdering = Ordering.by( (l : Client.Log.Recorded) => (l.blockNumber, l.transactionIndex, l.logIndex) )
    val publishables = confirmed.foldLeft( immutable.TreeSet.empty[Client.Log.Recorded]( logOrdering ) ){ ( accum, next ) =>
      val ( _, blockMap ) = next
      accum ++ blockMap.values
    }
    subs.foreach( _.enqueue( publishables.toVector, false ) )
  }

  private def terminate( mbt : Option[Throwable] ) : Unit = this.synchronized {
    cancelSubscriptions()
    def doTerminate : SimpleSubscription[Client.Log.Recorded] => Unit = {
      mbt match {
        case Some( t ) => ( s : SimpleSubscription[Client.Log.Recorded] ) => s.break(t)
        case None      => ( s : SimpleSubscription[Client.Log.Recorded] ) => s.complete()
      }
    }
    subscriptions.foreach( doTerminate )
    removeAllSubscriptions()
  }

  private def break( t : Throwable ) : Unit = terminate( Some( t ) )

  private def streamComplete() : Unit = terminate( None )

  private final object LogSubscriber extends RxSubscriber[Client.Log] {
    def onSubscribe( s : RxSubscription ) : Unit = ConfirmedLogPublisher.this.synchronized {
      logSubscription = s
    }
    def onNext( log : Client.Log ) : Unit = newLog( log )
    def onError( t : Throwable ) : Unit   = break( t )
    def onComplete() : Unit               = streamComplete()
  }

  private final object BlockNumSubscriber extends RxSubscriber[BigInt] {
    def onSubscribe( s : RxSubscription ) : Unit = ConfirmedLogPublisher.this.synchronized {
      numSubscription = s
    }
    def onNext( blockNum : BigInt ) : Unit = newBlockNum( blockNum )
    def onError( t : Throwable ) : Unit    = break( t )
    def onComplete() : Unit                = break( new Exception("Unexpected end of neverending block number stream!") )
  }

}


