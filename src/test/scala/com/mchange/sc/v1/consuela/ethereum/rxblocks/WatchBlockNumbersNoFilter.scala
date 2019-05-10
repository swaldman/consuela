package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

object WatchBlockNumbersNoFilter {
  def main( argv : Array[String] ) : Unit = {
    val ethJsonRpcUrl = argv(0)
    val publisher = new BlockNumberNoFilterPublisher( ethJsonRpcUrl )
    val subscriber = new TestSubscriber() {
      override def onComplete() : Unit = {
        super.onComplete()
        WatchBlockNumbersNoFilter.synchronized{ WatchBlockNumbersNoFilter.notifyAll() }
      }
      override def onError( t : Throwable ) : Unit = {
        super.onError( t )
        WatchBlockNumbersNoFilter.synchronized{ WatchBlockNumbersNoFilter.notifyAll() }
      }
    }
    publisher.subscribe( subscriber )
    WatchBlockNumbersNoFilter.synchronized{ WatchBlockNumbersNoFilter.wait() }
  }
}
