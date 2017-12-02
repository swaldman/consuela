package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

object WatchBlockNumbers {
  def main( argv : Array[String] ) : Unit = {
    val ethJsonRpcUrl = argv(0)
    val publisher = new BlockNumberPublisher( ethJsonRpcUrl )
    val subscriber = new TestSubscriber() {
      override def onComplete() : Unit = {
        super.onComplete()
        WatchBlockNumbers.synchronized{ WatchBlockNumbers.notifyAll() }
      }
      override def onError( t : Throwable ) : Unit = {
        super.onError( t )
        WatchBlockNumbers.synchronized{ WatchBlockNumbers.notifyAll() }
      }
    }
    publisher.subscribe( subscriber )
    WatchBlockNumbers.synchronized{ WatchBlockNumbers.wait() }
  }
}
