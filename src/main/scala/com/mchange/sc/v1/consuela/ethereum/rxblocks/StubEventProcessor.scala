package com.mchange.sc.v1.consuela.ethereum.rxblocks

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Abi,Client}
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent
import com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v2.concurrent.Scheduler

import com.mchange.sc.v2.failable._

final class StubEventTransformer( abi : Abi, subscriptionUpdateDelay : Duration = 3.seconds )( implicit
  scheduler : Scheduler,
  executionContext : ExecutionContext
) extends SimpleTransformer[Client.Log.Recorded,(SolidityEvent, stub.Event.Metadata)]( subscriptionUpdateDelay )( scheduler, executionContext ) {

  val interpretor = SolidityEvent.Interpretor( abi )

  def ftransform( recorded : Client.Log.Recorded ) : Failable[ (SolidityEvent, stub.Event.Metadata) ] = {
    for {
      solidityEvent <- interpretor.interpret( recorded.ethLogEntry )
    }
    yield {
      ( solidityEvent, stub.Event.Metadata( recorded ) )
    }
  }
}
