/*
package com.mchange.sc.v1.consuela.ethereum.rxblocks

import scala.collection._

class ConfirmedLogPublisher( ethJsonRpcUrl : String, query : Client.LogFilter.Query, confirmations : Int, blockPollDelay : Duration = 3.seconds, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  cfactory                 : Client.Factory   = Client.Factory.Default,
  scheduler                : Scheduler        = Scheduler.Default,
  executionContext         : ExecutionContext = ExecutionContext.global
) extends Publisher[Client.Log.Recorded] {

  //MT: protected by this' lock
  val pendingConfirmtions = mutable.TreeMap.empty[Unsigned256,mutable.HashMap[EthHash,Client.Log.Recorded]

}
*/

