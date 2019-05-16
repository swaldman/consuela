package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection._
import scala.concurrent.{ExecutionContext,Future}
import com.mchange.sc.v1.consuela.ethereum.{ethabi,jsonrpc,EthAddress,EthHash,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256
import jsonrpc.Client
import ethabi.SolidityEvent

import com.mchange.sc.v1.log.MLevel._
import com.mchange.sc.v3.failable._
import com.mchange.sc.v3.failable.logging._

object Event {
  implicit lazy val logger = mlogger( this )

  final object Metadata {
    def apply( recorded : Client.Log.Recorded ) : Metadata = {
      this.apply (
        logIndex         = recorded.logIndex,
        transactionIndex = recorded.transactionIndex,
        transactionHash  = recorded.transactionHash,
        blockHash        = recorded.blockHash,
        blockNumber      = recorded.blockNumber
      )
    }
    private[Event]
    def apply( logIndex : Unsigned256, transactionHash : EthHash, details : TransactionInfo.Details ) : Metadata = {
      this.apply (
        logIndex         = logIndex,
        transactionIndex = details.transactionIndex,
        transactionHash  = transactionHash,
        blockHash        = details.blockHash,
        blockNumber      = details.blockNumber
      )
    }
  }
  final case class Metadata (
    val logIndex         : Unsigned256,
    val transactionIndex : Unsigned256,
    val transactionHash  : EthHash,
    val blockHash        : EthHash,
    val blockNumber      : Unsigned256
  )

  def collectForAbi[T <: Event]( abi : jsonrpc.Abi, transactionHash : EthHash, details : TransactionInfo.Details, factory : ( SolidityEvent, Metadata ) => T ) : immutable.Seq[T] = {
    val interpretor = SolidityEvent.Interpretor( abi )
    val factupled = factory.tupled
    val failables = details.logs.zip( Stream.from(0) ).map {
      case( log, index ) => interpretor.interpret( log ).map( se => ( se, Metadata.apply( Unsigned256(index), transactionHash, details ) ) ).map( factupled )
    }
    val ( good, bad ) = failables.partition( _.isSucceeded )
    bad.foreach( _.xinfo( premessage = "Event uninterpretable for ABI: " ) )
    good.map( _.asSucceeded.assert )
  }

  def collectForAbi[T <: Event]( abi : jsonrpc.Abi, info : TransactionInfo, factory : ( SolidityEvent, Metadata ) => T )  : immutable.Seq[T] = {
    collectForAbi( abi, info.transactionHash, info.details, factory )
  }

  def collectForAbi[T <: Event]( abi : jsonrpc.Abi, info : TransactionInfo.Async, factory : ( SolidityEvent, Metadata ) => T )( implicit ec : ExecutionContext )  : Future[immutable.Seq[T]] = {
    info.details.map( deets => collectForAbi( abi, info.transactionHash, deets, factory ) )
  }
}
trait Event {
  def metadata : Event.Metadata
  def logEntry : EthLogEntry

  def sourceAddress : EthAddress = logEntry.address
}
