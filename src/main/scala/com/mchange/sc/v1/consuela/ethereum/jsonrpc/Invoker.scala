package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthChainId, EthHash, EthSigner, EthTransaction, EthereumException}
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

  def throwDisapproved( signed : EthTransaction.Signed, message : String = "Transaction aborted.", keepStackTrace : Boolean = true ) : Nothing = {
    val e = new TransactionDisapprovedException( signed, message )
    if ( !keepStackTrace ) e.setStackTrace( Array.empty )
    throw e
  }

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
      val ChainId             : Option[EthChainId]  = None
      val GasPriceTweak       : MarkupOrOverride    = Markup(0)
      val GasLimitTweak       : MarkupOrOverride    = Markup(0.2)
      val PollPeriod          : Duration            = 3.seconds
      val PollTimeout         : Duration            = Duration.Inf
      val HttpTimeout         : Duration            = Duration.Inf
      val TransactionApprover : TransactionApprover = AlwaysApprover
      val TransactionLogger   : TransactionLogger   = Invoker.TransactionLogger.None
      val ExchangerFactory    : Exchanger.Factory   = Exchanger.Factory.Default
      val Poller              : Poller              = com.mchange.sc.v2.concurrent.Poller.Default
      val ExecutionContext    : ExecutionContext    = scala.concurrent.ExecutionContext.global
    }
    final object Default extends Default

    def fromUrl[ U : URLSource ](
      jsonRpcUrl          : U,
      chainId             : Option[EthChainId]  = Default.ChainId,
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      httpTimeout         : Duration            = Default.HttpTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover,
      transactionLogger   : TransactionLogger   = Default.TransactionLogger
    )( implicit efactory : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( LoadBalancer.Single( jsonRpcUrl ), chainId, gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, httpTimeout, transactionApprover, transactionLogger, efactory, poller, econtext )
    }
    def fromUrls[ U : URLSource ](
      jsonRpcUrls         : immutable.Iterable[U],
      chainId             : Option[EthChainId]  = Default.ChainId,
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      httpTimeout         : Duration            = Default.HttpTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover, 
      transactionLogger   : TransactionLogger = Default.TransactionLogger
    )( implicit efactory : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( LoadBalancer.RoundRobin( jsonRpcUrls ), chainId, gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, httpTimeout, transactionApprover, transactionLogger, efactory, poller, econtext )
    }
    def fromLoadBalancer (
      loadBalancer        : LoadBalancer,
      chainId             : Option[EthChainId]  = Default.ChainId,
      gasPriceTweak       : MarkupOrOverride    = Default.GasPriceTweak,
      gasLimitTweak       : MarkupOrOverride    = Default.GasLimitTweak,
      pollPeriod          : Duration            = Default.PollPeriod, 
      pollTimeout         : Duration            = Default.PollTimeout,
      httpTimeout         : Duration            = Default.HttpTimeout,
      transactionApprover : TransactionApprover = Default.TransactionApprover, 
      transactionLogger   : TransactionLogger   = Default.TransactionLogger
    )( implicit efactory : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
      Context( loadBalancer, chainId, gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout, httpTimeout, transactionApprover, transactionLogger, efactory, poller, econtext )
    }
  }
  final case class Context(
    val loadBalancer        : LoadBalancer,
    val chainId             : Option[EthChainId],
    val gasPriceTweak       : MarkupOrOverride,
    val gasLimitTweak       : MarkupOrOverride,
    val pollPeriod          : Duration,
    val pollTimeout         : Duration,
    val httpTimeout         : Duration,
    val transactionApprover : TransactionApprover,
    val transactionLogger   : TransactionLogger,
    val efactory            : Exchanger.Factory,
    val poller              : Poller,
    val econtext            : ExecutionContext
  )

  private final case class NewClient( client : Client, url : URL ) extends AutoCloseable {
    def close() : Unit = client.close()
  }
  private def newClient( icontext : Invoker.Context ) : NewClient = {
    val url = icontext.loadBalancer.nextURL
    NewClient( Client.forExchanger( icontext.efactory( Exchanger.Config( url, icontext.httpTimeout ) ) ), url )
  }

  private def computedGas(
    client     : Client,
    from       : EthAddress,
    to         : Option[EthAddress],
    valueInWei : Unsigned256,
    data       : immutable.Seq[Byte]
  )(implicit icontext : Invoker.Context ) : Future[ComputedGas] = {

    implicit val econtext = icontext.econtext

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

  /**
    * If pollTimeout is not infinite, this future can fail with a Poller.TimeoutException
    */ 
  def futureTransactionReceipt(
    transactionHash : EthHash
  )( implicit icontext : Invoker.Context ) : Future[Client.TransactionReceipt] = {

    implicit val ( poller, econtext ) = ( icontext.poller, icontext.econtext )

    borrow( newClient( icontext ) ) { case NewClient(client, url) =>

      def repoll = {
        TRACE.log( s"Repolling for transaction receipt, transaction hash: 0x${transactionHash.hex}" )
        client.eth.getTransactionReceipt( transactionHash )
      }

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

      val task : Poller.Task[Client.TransactionReceipt] = {
        TRACE.logEval( new Poller.Task( s"Polling for transaction hash '0x${transactionHash.hex}'", icontext.pollPeriod, doPoll _, icontext.pollTimeout ) )
      }

      poller.addTask( task )
    }
  }

  def getBalance( address : EthAddress )( implicit icontext : Invoker.Context ) : Future[BigInt] = {
    implicit val econtext = icontext.econtext

    borrow( newClient( icontext ) ) { case NewClient(client, url) =>
      client.eth.getBalance( address, Client.BlockNumber.Latest )
    }
  }

  def getLogs( query : Client.Log.Filter.Query )( implicit icontext : Invoker.Context ) : Future[immutable.Seq[Client.Log]] = {
    implicit val econtext = icontext.econtext

    borrow( newClient( icontext ) ) { case NewClient(client, url) =>
      client.eth.getLogs( query )
    }
  }

  final object transaction {

    def sendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      forceNonce    : Option[Unsigned256] = None
    )( implicit icontext : Invoker.Context ) : Future[EthHash] = {
      sendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte], forceNonce)( icontext )
    }

    def sendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthHash] = {
      implicit val econtext = icontext.econtext

      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
        for {
          signed <- _prepareSendMessage( client, url )( senderSigner, to, valueInWei, data, forceNonce )( icontext )
          hash   <- _sendSignedTransaction( client, url )( signed )( icontext )
        }
        yield {
          hash
        }
      }
    }

    def createContract(
      creatorSigner : EthSigner,
      valueInWei    : Unsigned256,
      init          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthHash] = {

      implicit val econtext = icontext.econtext

      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
        for {
          signed <- _prepareCreateContract( client, url )( creatorSigner, valueInWei, init, forceNonce )( icontext )
          hash   <- _sendSignedTransaction( client, url )( signed )( icontext )
        }
        yield {
          hash
        }
      }
    }

    def sendSignedTransaction( signed : EthTransaction.Signed )(implicit icontext : Invoker.Context ) : Future[EthHash] = {
      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
        _sendSignedTransaction( client, url)( signed )( icontext )
      }
    }

    def prepareSendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      forceNonce    : Option[Unsigned256] = None
    )( implicit icontext : Invoker.Context ) : Future[EthTransaction.Signed] = {
      prepareSendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte], forceNonce)( icontext )
    }

    def prepareSendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthTransaction.Signed] = {
      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
        _prepareSendMessage( client, url )( senderSigner, to, valueInWei, data, forceNonce )( icontext )        
      }
    }

    def prepareCreateContract(
      creatorSigner : EthSigner,
      valueInWei    : Unsigned256,
      init          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthTransaction.Signed] = {
      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
        _prepareCreateContract( client, url )( creatorSigner, valueInWei, init, forceNonce )
      }
    }

    private def _prepareSendMessage( client : Client, url : URL )(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthTransaction.Signed] = {

      implicit val econtext = icontext.econtext

      val from = senderSigner.address

      val fComputedGas = computedGas( client, from, Some(to), valueInWei, data )
      val fNextNonce = {
        forceNonce match {
          case Some( nonce ) => Future.successful( nonce )
          case None          => client.eth.getTransactionCount( from, Client.BlockNumber.Pending ).map( Unsigned256.apply )
        }
      }

      for {
        cg <- fComputedGas
        nextNonce <- fNextNonce
        unsigned = TRACE.logEval( "Message transaction" )( EthTransaction.Unsigned.Message( nextNonce, Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), to, valueInWei, data ) )
      }
      yield {
        unsigned.sign( senderSigner, icontext.chainId )
      }
    }

    private def _prepareCreateContract( client : Client, url : URL )(
      creatorSigner : EthSigner,
      valueInWei    : Unsigned256,
      init          : immutable.Seq[Byte],
      forceNonce    : Option[Unsigned256] = None
    )(implicit icontext : Invoker.Context ) : Future[EthTransaction.Signed] = {
      implicit val econtext = icontext.econtext

      val from = creatorSigner.address

      val fComputedGas = computedGas( client, from, None, valueInWei, init )
      val fNextNonce = {
        forceNonce match {
          case Some( nonce ) => Future.successful( nonce )
          case None          => client.eth.getTransactionCount( from, Client.BlockNumber.Pending ).map( Unsigned256.apply )
        }
      }

      for {
        cg <- fComputedGas
        nextNonce <- fNextNonce
        unsigned = TRACE.logEval("Contract creation transaction")( EthTransaction.Unsigned.ContractCreation( nextNonce, Unsigned256(cg.gasPrice), Unsigned256(cg.gasLimit), valueInWei, init ) )
      }
      yield {
        unsigned.sign( creatorSigner, icontext.chainId )
      }
    }

    private def _sendSignedTransaction( client : Client, url : URL )( signed : EthTransaction.Signed )(implicit icontext : Invoker.Context ) : Future[EthHash] = {
      implicit val econtext = icontext.econtext
      for {
        _ <- icontext.transactionApprover( signed )
        hash <- client.eth.sendSignedTransaction( signed )
      } yield {
        TransactionLogger.log( icontext.transactionLogger, hash, signed, url.toExternalForm )
        hash
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
      implicit val ( poller, econtext ) = ( icontext.poller, icontext.econtext )

      borrow( newClient( icontext ) ) { case NewClient(client, url) =>
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
