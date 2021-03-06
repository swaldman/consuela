package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

object WatchAddressLogsNoFilter {
  def main( argv : Array[String] ) : Unit = {
    val (ethJsonRpcUrl, address, toBlock) = argv.length match {
      case 2 => ( argv(0), EthAddress( argv(1) ), Client.BlockNumber.Latest )
      case 3 => ( argv(0), EthAddress( argv(1) ), Client.BlockNumber.Quantity( BigInt(argv(2)) ) )
      case _ => throw new Exception( s"""${this} requires a jsonrpc URL and a hex address. A terminal blocknumber can also be supplied. Nothing else is permitted. ${argv.mkString(",")}""" )
    }
    val query = Client.Log.Filter.Query( addresses = Seq(address), fromBlock = Some(Client.BlockNumber.Earliest), toBlock = Some(toBlock) )
    val logPublisher = new LogNoFilterPublisher( ethJsonRpcUrl, query )
    val subscriber = new TestSubscriber() {
      override def onComplete() : Unit = {
        super.onComplete()
        WatchAddressLogsNoFilter.synchronized{ WatchAddressLogsNoFilter.notifyAll() }
      }
      override def onError( t : Throwable ) : Unit = {
        super.onError( t )
        WatchAddressLogsNoFilter.synchronized{ WatchAddressLogsNoFilter.notifyAll() }
      }
    }
    logPublisher.subscribe( subscriber )
    WatchAddressLogsNoFilter.synchronized{ WatchAddressLogsNoFilter.wait() }
  }
}
