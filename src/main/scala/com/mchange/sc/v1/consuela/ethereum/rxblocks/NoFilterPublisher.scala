package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v2.jsonrpc.Exchanger

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

import com.mchange.sc.v2.concurrent.Scheduler
import com.mchange.sc.v2.lang._

import com.mchange.sc.v3.failable._
import com.mchange.sc.v3.failable.logging._

import org.reactivestreams.{Publisher => RxPublisher, Subscriber => RxSubscriber, Subscription => RxSubscription}

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._
import scala.util.Failure

import java.util.concurrent.atomic.AtomicReference

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
object NoFilterPublisher {
  private [NoFilterPublisher] implicit lazy val logger = mlogger( this )

  final case class Transformed[T]( items : immutable.Seq[T], shouldTerminate : Boolean ) // we don't make inner because of annoying outer reference issues and warnings

  private final val Zero = BigInt(0)
}
abstract class NoFilterPublisher[T, S]( ethJsonRpcUrl : String, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  efactory                 : Exchanger.Factory = Exchanger.Factory.Default,
  scheduler                : Scheduler         = Scheduler.Default,
  executionContext         : ExecutionContext  = ExecutionContext.global // XXX: SHould we reorganize so that by default we use the Scheduler's thread pool?
) extends RxPublisher[T] with SimpleSubscription.Parent[T] {

  import NoFilterPublisher.{logger, Transformed, Zero}

  private val lastCheckedBlockNumber = new AtomicReference[BigInt]( BigInt(-1) )

  protected def getChanges( client : Client, fromBlock : BigInt, toBlock : BigInt ) : Future[immutable.Seq[S]]

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

  private [rxblocks] def removeSubscription( subscription : SimpleSubscription[T] ) = this.synchronized {
    subscriptions -= subscription
    if ( subscriptions.isEmpty ) shutdownPolling()
  }

  // call only while holding this' lock
  private def startupPolling() = {
    try {
      this._client = Client.forExchanger( efactory( ethJsonRpcUrl ) )
      this.scheduled = scheduler.scheduleWithFixedDelay( doPoll _, 0.seconds, blockPollDelay )
    }
    catch {
      case t : Throwable => {
        shutdownPolling()
        throw t
      }
    }
  }

  private def doPoll() : Unit = {
    val ( c, sl ) = this.synchronized { (_client, subscriptions.toList) }
    val lastChecked = lastCheckedBlockNumber.get()
    val f_blockNumber = c.eth.blockNumber();
    f_blockNumber map { curBlockNumber  =>
      TRACE.log( s"Current block number: ${curBlockNumber}, lastChecked: ${lastChecked}" )
      if ( lastChecked >= 0 && curBlockNumber >= lastChecked ) {
        if ( curBlockNumber > lastChecked ) { // only actually repoll if the block number has advanced
          val f_broadcast = {
            for {
              items <- getChanges( c, lastChecked + 1, curBlockNumber ) // getChanges(...) is inclusive, don't include the block we've already seen.
              transformed <- transformTerminate( c, items )
            } yield {
              sl.foreach( _.enqueue( transformed.items, transformed.shouldTerminate ) )
            }
          } recover { case t =>
              SEVERE.log("An error occured while trying to poll for items to publish!", t )
              sl.foreach( _.break( t ) )
              SEVERE.log( s"${this} shutting down due to error." )
              shutdownPolling()
          } onComplete { _=>
            TRACE.log( s"Setting last checked to ${curBlockNumber}." )
            lastCheckedBlockNumber.set( curBlockNumber )
          }
        }
        else {
          TRACE.log( "Nothing to update!" )
        }
      }
      else {
        val setTo = Zero.max(curBlockNumber - 1) // we want to see events fron the upon-initiaization current block
        TRACE.log( s"Setting first-to-check to ${curBlockNumber} (last checked to ${setTo})." )
        lastCheckedBlockNumber.set( setTo )
      }
    }
  }

  private def shutdownPolling() = this.synchronized {
    if ( scheduled != null ) {
      Failable( scheduled.attemptCancel() ).xwarn("Exception while canceling polling")
    }
    this.scheduled = null
    this._client = null
  }
}
