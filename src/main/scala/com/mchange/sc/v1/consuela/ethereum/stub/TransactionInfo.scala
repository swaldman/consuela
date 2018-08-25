package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection.immutable
import scala.concurrent.{Await,ExecutionContext,Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure,Success,Try}
import com.mchange.sc.v1.consuela.ethereum.{EthHash,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client
import com.mchange.sc.v2.concurrent.Poller

object TransactionInfo {

  final class TimeoutException( transactionHash : EthHash, cause : Throwable ) extends StubException( s"Timed out while awaiting a receipt for transaction with hash '0x${transactionHash.hex}'.", cause )

  sealed trait Base {
    def transactionHash : EthHash
  }

  case class Details (
    transactionIndex  : sol.UInt256,
    blockHash         : EthHash,
    blockNumber       : sol.UInt256,
    cumulativeGasUsed : sol.UInt256,
    gasUsed           : sol.UInt256,
    logs              : immutable.Seq[EthLogEntry]
  )
  
  final object Async {
    def fromClientTransactionReceipt( transactionHash : EthHash, fReceipt : Future[Client.TransactionReceipt] )( implicit ec : ExecutionContext ) : TransactionInfo.Async = {
      val fdetails = fReceipt.map( ctr => TransactionInfo.Details( ctr.transactionIndex, ctr.blockHash, ctr.blockNumber, ctr.cumulativeGasUsed, ctr.gasUsed, ctr.logs ) )
      this.apply( transactionHash, fdetails )
    }
  }
  case class Async (
    val transactionHash : EthHash,
    val details         : Future[TransactionInfo.Details]
  ) extends Base {
    def await : TransactionInfo = {
      Await.ready( details, Duration.Inf ).value match { // the Poller times out internally
        case Some( Success( deets ) )                       => TransactionInfo( transactionHash, deets )
        case Some( Failure( t : Poller.TimeoutException ) ) => throw new TimeoutException( transactionHash, t )
        case Some( Failure( t ) )                           => throw t
        case None                                           => throw new Exception( s"Huh? Return from Await.ready( ..., Duration.Inf ) without a value???" )
      }
    }
  }
}
case class TransactionInfo (
  val transactionHash : EthHash,
  val details         : TransactionInfo.Details
) extends TransactionInfo.Base

