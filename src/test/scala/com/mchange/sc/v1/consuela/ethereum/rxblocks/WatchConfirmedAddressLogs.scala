package com.mchange.sc.v1.consuela.ethereum.rxblocks

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client

object WatchConfirmedAddressLogs {
  val DefaultNumConfirmations = 10

  def main( argv : Array[String] ) : Unit = {
    val (ethJsonRpcUrl, address, toBlock, numConfirmations) = argv.length match {
      case 2 => ( argv(0), EthAddress( argv(1) ), Client.BlockNumber.Latest, DefaultNumConfirmations )
      case 3 => ( argv(0), EthAddress( argv(1) ), Client.BlockNumber.Quantity( BigInt(argv(2)) ), DefaultNumConfirmations )
      case 4 => ( argv(0), EthAddress( argv(1) ), Client.BlockNumber.Quantity( BigInt(argv(2)) ), argv(3).toInt )
      case _ => throw new Exception( s"""${this} requires a jsonrpc URL and a hex address. A terminal blocknumber then a number of confirmations can also be supplied. Nothing else is permitted. ${argv.mkString(",")}""" )
    }
    val query = Client.Log.Filter.Query( addresses = Seq(address), fromBlock = Some(Client.BlockNumber.Earliest), toBlock = Some(toBlock) )
    val confirmedLogPublisher = new ConfirmedLogPublisher( ethJsonRpcUrl, query, numConfirmations )
    val subscriber = new TestSubscriber() {
      override def onComplete() : Unit = {
        super.onComplete()
        WatchConfirmedAddressLogs.synchronized{ WatchConfirmedAddressLogs.notifyAll() }
      }
      override def onError( t : Throwable ) : Unit = {
        super.onError( t )
        WatchConfirmedAddressLogs.synchronized{ WatchConfirmedAddressLogs.notifyAll() }
      }
    }
    confirmedLogPublisher.subscribe( subscriber )
    WatchConfirmedAddressLogs.synchronized{ WatchConfirmedAddressLogs.wait() }
  }
}
