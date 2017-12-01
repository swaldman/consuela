package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client
import Client.Filter

import com.mchange.sc.v2.concurrent.Scheduler
import com.mchange.sc.v2.failable._
import com.mchange.sc.v2.lang._

import org.reactivestreams.{Publisher => RxPublisher, Subscriber => RxSubscriber, Subscription => RxSubscription}

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._
import scala.util.Failure

/**
  * Unless the chosen filter supports a "fromBlock", there is no way to control precisely at what
  * block a subscription begins. A subscriber receives events "as they happen", and even if
  * they subscribe very quickly after e.g. contract deployment, there can be no guarantee that
  * they won't miss anything.
  * 
  * Fortunately, the most important filters (log filters) do support "fromBlock"
  * 
  * Unfortunately, it seems that clients don't pay attantion to an already-past "fromBlock" in
  * the eth_getFilterChanges most implementations will rly upon.
  * 
  * T is the type that subscribers will receive.
  * S is an intermediate type, that might be acquired then transformed to T. If this is not necessary,
  *   just define S to the same type as T.
  * F is the Client.Filter type of the object usually used to specify the items that will get
  *   published. (Filter.Dummy can be acquired and left unused if no filter is necessary.)
  */
object SimplePublisher {
  private [SimplePublisher] implicit lazy val logger = mlogger( this )

  final case class Transformed[T]( items : immutable.Seq[T], shouldTerminate : Boolean ) // we don't make inner because of annoying outer reference issues and warnings
}
abstract class SimplePublisher[T, S, F <: Filter]( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : Client.Factory   = Client.Factory.Default,
  scheduler                : Scheduler        = Scheduler.Default,
  executionContext         : ExecutionContext = ExecutionContext.global // XXX: SHould we reorganize so that by default we use the Scheduler's thread pool?
) extends RxPublisher[T] with SimpleSubscription.Parent[T] {

  import SimplePublisher.{logger, Transformed}

  protected def acquireFilter( client : Client )          : Future[F]
  protected def getChanges( client : Client, filter : F ) : Future[immutable.Seq[S]]

  /**
    * This is an opportunity to transform acquired items -- to filter items that should not be published, to map items into something else
    * etc. If the stream should terminate, excess items (thise beyond the termination point) should be trimmed.
    * 
    * At the same time it is a place where termination can be noticed, by inspecting acquired items. Signal termination
    * by setting `transformed.shouldTerminate` to true.
    * 
    * A client is provided (primarily in case it is helpful for deciding termination).
    * 
    * In the simple case where T and S are the same type, no transformation need happen,
    * and the stream of events is unbounded, a straightforward implementation is 
    * Future.successful( Transformed[T]( items, false ) )
    * 
    * See BlockHashPublisher for an example.
    */ 
  protected def transformTerminate( client : Client, items : immutable.Seq[S] ) : Future[Transformed[T]]

  protected def cancelFilter( client : Client, filter : F ) : Future[_] = {
    if ( filter != Client.Filter.Dummy ) {
      client.eth.uninstallFilter( filter )
    } else {
      Future.successful( () )
    }
  }

  def subscribe( subscriber : RxSubscriber[_ >: T] ) : Unit = {
    val subscription = this.synchronized {
      val s = new SimpleSubscription[T]( this, subscriber )
      subscriptions += s

      // startup polling, enqueuing, dequeuing
      if (scheduled == null) startupPolling()

      s
    }
    subscriber.onSubscribe( subscription )
  }

  //MT: Protected by this' lock
  private val subscriptions : mutable.Set[SimpleSubscription[T]] = mutable.HashSet.empty[SimpleSubscription[T]]
  private var scheduled     : Scheduler.Scheduled[Unit] = null
  private var _client       : Client                    = null
  private var f_filter      : Future[F]                 = null

  private [rxblocks] def removeSubscription( subscription : SimpleSubscription[T] ) = this.synchronized {
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

  private def doPoll() : Unit= {
    val ( c, ff, sl ) = this.synchronized { (_client, f_filter, subscriptions.toList) }
    val pollFuture = {
      for {
        filter <- ff
        items <- getChanges( c, filter )
        transformed <- transformTerminate( c, items )
      } yield {
        sl.foreach( _.enqueue( transformed.items, transformed.shouldTerminate ) )
      }
    }
    pollFuture recover { case t =>
      SEVERE.log("An error occured while trying to poll for items to publish!", t )
      sl.foreach( _.break( t ) )
      SEVERE.log( s"${this} shutting down due to error." )
      shutdownPolling()
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
}
