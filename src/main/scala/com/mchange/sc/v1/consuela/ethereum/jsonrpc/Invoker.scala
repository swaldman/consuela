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
  final class TransactionDisapprovedException( val transaction : EthTransaction.Signed, message : String )
      extends InvokerException( message + s" [Disapproved: gasPrice -> ${transaction.gasPrice}, gasLimit -> ${transaction.gasLimit}, valueInWei -> ${transaction.value}]" )

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

  final case class ComputedGas( gasPrice : BigInt, gasLimit : BigInt )

  def throwDisapproved( signed : EthTransaction.Signed, message : String ) : Nothing = throw new TransactionDisapprovedException( signed, message )
  def throwDisapproved( signed : EthTransaction.Signed )                   : Nothing = throwDisapproved( signed, "Transaction aborted." )


  type TransactionApprover = EthTransaction.Signed => Future[Unit] // a failure, usually a TransactionDisapprovedException, signifies disapproval

  val AlwaysApprover : TransactionApprover = _ => Future.successful( () )

  final case class TransactionLogEntry( transactionHash : EthHash, transaction : EthTransaction.Signed, jsonRpcUrl : String )

  final object TransactionLogger {
    final case object None extends TransactionLogger {
      def apply( entry : TransactionLogEntry, ec : ExecutionContext ) : Future[Unit] = throw new NotImplementedError( "TransactionLogger.None is a token; it should never actually be called." )
    }
    private [Invoker] def log( tlogger : TransactionLogger, transactionHash : EthHash, transaction : EthTransaction.Signed, jsonRpcUrl : String )( implicit ec : ExecutionContext ) : Future[Unit] = {
      if ( tlogger != TransactionLogger.None ) {
        val entry = new TransactionLogEntry( transactionHash, transaction, jsonRpcUrl )
        tlogger( entry, ec ) recover { case t : Throwable =>
          WARNING.log( s"Failed to log a transaction with specified TransactionLogger. Unlogger entry: ${entry}", t )
        }
      } else {
        Future.successful( () )
      }
    }
  }
  type TransactionLogger = (TransactionLogEntry, ExecutionContext) => Future[Unit]


  object Context {
    // we define these in a trait so that e.g. stub.Context can easily steal them
    trait Default {
      val GasPriceTweak       : MarkupOrOverride    = Markup(0)
      val GasLimitTweak       : MarkupOrOverride    = Markup(0.2)
      val PollPeriod          : Duration            = 3.seconds
      val PollTimeout         : Duration            = Duration.Inf
      val TransactionApprover : TransactionApprover = AlwaysApprover
      val TransactionLogger   : TransactionLogger   = Invoker.TransactionLogger.None
      val ClientFactory       : Client.Factory      = Client.Factory.Default
      val Poller              : Poller              = com.mchange.sc.v2.concurrent.Poller.Default
      val ExecutionContext    : ExecutionContext    = scala.concurrent.ExecutionContext.global
    }
    final object Default extends Default

    def fromUrl[ U : URLSource ](
      jsonRpcUrl          : U,
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover,
      transactionLogger   : TransactionLogger   = Default.TransactionLogger
    )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( LoadBalancer.Single( jsonRpcUrl ), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, transactionApprover, transactionLogger, cfactory, poller, econtext )
    }
    def fromUrls[ U : URLSource ](
      jsonRpcUrls         : immutable.Iterable[U],
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover, 
      transactionLogger   : TransactionLogger = Default.TransactionLogger
    )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( LoadBalancer.RoundRobin( jsonRpcUrls ), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, transactionApprover, transactionLogger, cfactory, poller, econtext )
    }
    def fromLoadBalancer (
      loadBalancer        : LoadBalancer,
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover, 
      transactionLogger   : TransactionLogger   = Default.TransactionLogger
    )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( loadBalancer, gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, transactionApprover, transactionLogger, cfactory, poller, econtext )
    }
  }
  final case class Context(
    val loadBalancer        : LoadBalancer,
    val gasPriceTweak       : MarkupOrOverride,
    val gasLimitTweak       : MarkupOrOverride,
    val pollPeriod          : Duration,
    val pollTimeout         : Duration,
    val transactionApprover : TransactionApprover,
    val transactionLogger   : TransactionLogger,
    val cfactory            : Client.Factory,
    val poller              : Poller,
    val econtext            : ExecutionContext
  )

  private def computedGas(
    client     : Client,
    from       : EthAddress,
    to         : Option[EthAddress],
    valueInWei : Unsigned256,
    data       : immutable.Seq[Byte]
  )(implicit icontext : Invoker.Context ) : Future[ComputedGas] = {

    implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )

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
  )( implicit icontext : Invoker.Context ) : Future[Option[Client.TransactionReceipt]] = {

    implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )

    borrow( cfactory( icontext.loadBalancer.nextURL ) ) { client =>

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

  def awaitTransactionReceipt( transactionHash : EthHash )( implicit icontext : Invoker.Context ) : Option[Client.TransactionReceipt] = {
    implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )
    Await.result( futureTransactionReceipt( transactionHash ), Duration.Inf ) // the timeout is enforced within futureTransactionReceipt( ... ), not here
  }

  def requireTransactionReceipt( transactionHash : EthHash )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Client.TransactionReceipt = {
    implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )
    awaitTransactionReceipt( transactionHash ).getOrElse( throw new TimeoutException( transactionHash, icontext.pollTimeout ) )
  }

  def getBalance( address : EthAddress )( implicit icontext : Invoker.Context ) : Future[BigInt] = {
      implicit val ( cfactory, econtext ) = ( icontext.cfactory, icontext.econtext )

      borrow( cfactory( icontext.loadBalancer.nextURL ) ) { client =>
        client.eth.getBalance( address, Client.BlockNumber.Latest )
      }
  }

  final object transaction {

    def sendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256
    )( implicit icontext : Invoker.Context ) : Future[EthHash] = {
      sendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte])( icontext )
    }

    def sendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context ) : Future[EthHash] = {
      implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )

      val url = icontext.loadBalancer.nextURL
      borrow( cfactory( url ) ) { client =>

        val from = senderSigner.address

        val fComputedGas = computedGas( client, from, Some(to), valueInWei, data )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          cg <- fComputedGas
          nextNonce <- fNextNonce
          unsigned = TRACE.logEval( "Message transaction" )( EthTransaction.Unsigned.Message( Unsigned256(nextNonce), Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), to, valueInWei, data ) )
          signed = unsigned.sign( senderSigner )
          _ <- icontext.transactionApprover( signed )
          hash <- client.eth.sendSignedTransaction( signed )
        } yield {
          TransactionLogger.log( icontext.transactionLogger, hash, signed, url.toExternalForm )
          hash
        }
      }
    }
    def createContract(
      creatorSigner : EthSigner,
      valueInWei    : Unsigned256,
      init          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context ) : Future[EthHash] = {
      implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )

      val url = icontext.loadBalancer.nextURL
      borrow( cfactory( url ) ) { client =>

        val from = creatorSigner.address

        val fComputedGas = computedGas( client, from, None, valueInWei, init )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          cg <- fComputedGas
          nextNonce <- fNextNonce
          unsigned = TRACE.logEval("Contract creation transaction")( EthTransaction.Unsigned.ContractCreation( Unsigned256(nextNonce), Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), valueInWei, init ) )
          signed = unsigned.sign( creatorSigner )
          _ <- icontext.transactionApprover( signed )
          hash <- client.eth.sendSignedTransaction( signed )
        } yield {
          TransactionLogger.log( icontext.transactionLogger, hash, signed, url.toExternalForm )
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
    )( implicit icontext : Invoker.Context ) : Future[immutable.Seq[Byte]] = {
      implicit val ( cfactory, poller, econtext ) = ( icontext.cfactory, icontext.poller, icontext.econtext )

      borrow( cfactory( icontext.loadBalancer.nextURL ) ) { client =>
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
