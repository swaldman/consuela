package com.mchange.sc.v1.consuela.ethereum.rxblocks

import org.reactivestreams._

class TestSubscriber( initRequest : Long = Long.MaxValue ) extends Subscriber[Any] {

  //MT: protected by this' lock
  var subscription : Subscription = null

  def onSubscribe( s : Subscription ) : Unit = this.synchronized {
    println( s"onSubscribe( $s )" )
    s.request( initRequest )
    this.subscription = s
  }
  def onNext( any : Any ) : Unit = {
    println( s"onNext( $any )" )
  }
  def onError( t : Throwable ) : Unit = {
    println( s"onError( $t )" )
    t.printStackTrace( System.out )
  }
  def onComplete() : Unit = {
    println( s"onComplete()" )
  }
  def requestMore( n : Long ) : Unit = this.synchronized {
    subscription.request( n )
  }
  def cancelSubscription() : Unit = this.synchronized {
    subscription.cancel()
  }
}
