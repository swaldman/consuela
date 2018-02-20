package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash, EthSigner, EthTransaction, EthereumException}
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v2.jsonrpc._

import com.mchange.sc.v2.concurrent.Poller

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v2.net.{URLSource,LoadBalancer}

import java.net.URL

import scala.annotation.tailrec

import scala.collection._

import scala.concurrent.{Await,ExecutionContext,Future,blocking}
import scala.concurrent.duration._

import scala.util.{Try,Success,Failure}

import java.util.concurrent.atomic.AtomicReference

object Invoker {

  private lazy implicit val logger = mlogger( this )

  class InvokerException( message : String, cause : Throwable = null ) extends EthereumException( message, cause )
  final class TimeoutException( transactionHash : EthHash, timeout : Duration ) extends InvokerException( s"Could not retrieve receipt for transaction '0x${transactionHash.hex}' within ${timeout}" )
  final class GasDisapprovedException( message : String, computedGas : ComputedGas ) extends InvokerException( message + s" [Disapproved: gasPrice -> ${computedGas.gasPrice}, gasLimit -> ${computedGas.gasLimit}]" )

  private def rounded( bd : BigDecimal ) = bd.round( bd.mc ) // work around absence of default rounded method in scala 2.10 BigDecimal

  final object MarkupOrOverride {
    def createMarkup( fraction : Double ) = Markup( fraction ) // we use createXXX to avoid using the bare keyword 'override' :(
    def createOverride( value : BigInt ) = Override( value )
    val None = Markup(0)
  }
  sealed trait MarkupOrOverride {
    def compute( default : BigInt ) : BigInt
  }
  final case class Markup( fraction : Double, cap : Option[BigInt] = None, floor : Option[BigInt] = None ) extends MarkupOrOverride {
    def compute( default : BigInt ) : BigInt = {
      val base             = rounded( BigDecimal( default ) * BigDecimal(1 + fraction) ).toBigInt
      val floored          = floor.fold( base )( floorInWei => base max floorInWei )
      val flooredAndCapped = cap.fold( floored )( capInWei => floored min capInWei )
      flooredAndCapped
    }
  }
  final case class Override( value : BigInt ) extends MarkupOrOverride {
    def compute( default : BigInt ) : BigInt = value
  }

  object Context {
    def fromUrl[ U : URLSource ](
      jsonRpcUrl : U,
      gasPriceTweak : MarkupOrOverride = Markup(0),
      gasLimitTweak : MarkupOrOverride = Markup(0.2),
      pollPeriod : Duration = 3.seconds,
      pollTimeout : Duration = Duration.Inf
    ) : Context = {
      this.apply( LoadBalancer.Single( jsonRpcUrl ), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout )
    }
    def fromUrls[ U : URLSource ](
      jsonRpcUrls : immutable.Iterable[U],
      gasPriceTweak : MarkupOrOverride = Markup(0),
      gasLimitTweak : MarkupOrOverride = Markup(0.2),
      pollPeriod : Duration = 3.seconds,
      pollTimeout : Duration = Duration.Inf
    ) : Context = {
      this.apply( LoadBalancer.Random( jsonRpcUrls ), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout )
    }
  }
  final case class Context( jsonRpcUrls : LoadBalancer, gasPriceTweak : MarkupOrOverride = Markup(0), gasLimitTweak : MarkupOrOverride = Markup(0.2), pollPeriod : Duration = 3.seconds, pollTimeout : Duration = Duration.Inf )

  final case class ComputedGas( gasPrice : BigInt, gasLimit : BigInt )

  type GasApprover = ComputedGas => Future[Unit] // a failure, usually a GasDisapprovedException, signifies disapproval

  val AlwaysApprover : GasApprover = _ => Future.successful( () )

  private def computedGas(
    client     : Client,
    from       : EthAddress,
    to         : Option[EthAddress],
    valueInWei : Unsigned256,
    data       : immutable.Seq[Byte]
  )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[ComputedGas] = {

    val fDefaultGasPrice = client.eth.gasPrice()
    val fDefaultGas      = client.eth.estimateGas( Some( from ), to, None, None, Some( valueInWei.widen ), Some ( data ) );

    for {
      defaultGasPrice <- fDefaultGasPrice
      effectiveGasPrice = icontext.gasPriceTweak.compute( defaultGasPrice )
      defaultGas <- fDefaultGas
      effectiveGas = icontext.gasLimitTweak.compute( defaultGas )
    } yield {
      ComputedGas( effectiveGasPrice, effectiveGas )
    }
  }

  def futureTransactionReceipt(
    transactionHash : EthHash
  )( implicit icontext : Invoker.Context, cfactory : Client.Factory, poller : Poller, econtext : ExecutionContext ) : Future[Option[Client.TransactionReceipt]] = {

    borrow( cfactory( icontext.jsonRpcUrls.nextURL ) ) { client =>

      def repoll = client.eth.getTransactionReceipt( transactionHash )

      val holder = new AtomicReference[Future[Option[Client.TransactionReceipt]]]( repoll )

      def doPoll() : Option[Client.TransactionReceipt] = {
        holder.get.value match {
          case None => {
            None // we just have to wait for the HTTP request to complete
          }
          case Some( Success( None ) ) => { // the HTTP request returned, we don't yet have the receipt, we have to repoll
            holder.set( repoll )
            None
          }
          case Some( Success( someClientTransactionReceipt ) ) => { // hooray, we found it!
            someClientTransactionReceipt
          }
          case Some( Failure( t ) ) => { // oops, something broke, we throw the Exception to our wrapping future
            throw t
          }
        }
      }

      val task = new Poller.Task( s"Polling for transaction hash '0x${transactionHash.hex}'", icontext.pollPeriod, doPoll _, icontext.pollTimeout )

      poller.addTask( task ).map( Some.apply ).recover {
        case e : Poller.TimeoutException => None 
      }
    }
  }

  def awaitTransactionReceipt( transactionHash : EthHash )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Option[Client.TransactionReceipt] = {
    Await.result( futureTransactionReceipt( transactionHash ), Duration.Inf ) // the timeout is enforced within futureTransactionReceipt( ... ), not here
  }

  def requireTransactionReceipt( transactionHash : EthHash )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Client.TransactionReceipt = {
    awaitTransactionReceipt( transactionHash ).getOrElse( throw new TimeoutException( transactionHash, icontext.pollTimeout ) )
  }


  final object transaction {

    def sendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext, gasApprover : GasApprover = AlwaysApprover ) : Future[EthHash] = {
      sendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte])( icontext, cfactory, econtext, gasApprover )
    }

    def sendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext, gasApprover : GasApprover = AlwaysApprover ) : Future[EthHash] = {
      borrow( cfactory( icontext.jsonRpcUrls.nextURL ) ) { client =>

        val from = senderSigner.address

        val fComputedGas = computedGas( client, from, Some(to), valueInWei, data )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          cg <- fComputedGas
          _ <- gasApprover(cg)
          nextNonce <- fNextNonce
          unsigned = TRACE.logEval( "Message transaction" )( EthTransaction.Unsigned.Message( Unsigned256(nextNonce), Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), to, valueInWei, data ) )
          signed = unsigned.sign( senderSigner )
          hash <- client.eth.sendSignedTransaction( signed )
        } yield {
          hash
        }
      }
    }
    def createContract(
      creatorSigner : EthSigner,
      valueInWei    : Unsigned256,
      init          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext, gasApprover : GasApprover = AlwaysApprover ) : Future[EthHash] = {
      borrow( cfactory( icontext.jsonRpcUrls.nextURL ) ) { client =>

        val from = creatorSigner.address

        val fComputedGas = computedGas( client, from, None, valueInWei, init )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          cg <- fComputedGas
          _ <- gasApprover(cg)
          nextNonce <- fNextNonce
          unsigned = TRACE.logEval("Contract creation transaction")( EthTransaction.Unsigned.ContractCreation( Unsigned256(nextNonce), Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), valueInWei, init ) )
          signed = unsigned.sign( creatorSigner )
          hash <- client.eth.sendSignedTransaction( signed )
        } yield {
          hash
        }
      }
    }
  }

  final object constant {
    def sendMessage(
      from       : EthAddress,
      to         : EthAddress,
      valueInWei : Unsigned256,
      data       : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext ) : Future[immutable.Seq[Byte]] = {
      borrow( cfactory( icontext.jsonRpcUrls.nextURL ) ) { client =>
        val fComputedGas = computedGas( client, from, Some(to), valueInWei, data )
        for {
          cg <- fComputedGas
          outBytes <- client.eth.call( Some( from ), Some( to ), Some( cg.gasLimit ), Some( cg.gasPrice ), Some( valueInWei.widen ), Some( data ) )
        } yield {
          outBytes
        }
      }
    }
  }
}
