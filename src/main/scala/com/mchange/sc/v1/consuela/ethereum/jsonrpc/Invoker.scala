package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash, EthSigner, EthTransaction, EthereumException}
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v2.jsonrpc._

import com.mchange.sc.v2.concurrent.Poller

import com.mchange.sc.v1.log.MLevel._

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

  final case class Context( jsonRpcUrl : String, gasPriceTweak : MarkupOrOverride = Markup(0), gasLimitTweak : MarkupOrOverride = Markup(0.2), pollPeriod : Duration = 3.seconds, pollTimeout : Duration = Duration.Inf )

  private def gasPriceGas(
    client     : Client,
    from       : EthAddress,
    to         : EthAddress,
    valueInWei : Unsigned256,
    data       : immutable.Seq[Byte]
  )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[( BigInt, BigInt )] = {

    val fDefaultGasPrice = client.eth.gasPrice()
    val fDefaultGas      = client.eth.estimateGas( Some( from ), Some( to ), None, None, Some( valueInWei.widen ), Some ( data ) );

    for {
      defaultGasPrice <- fDefaultGasPrice
      effectiveGasPrice = icontext.gasPriceTweak.compute( defaultGasPrice )
      defaultGas <- fDefaultGas
      effectiveGas = icontext.gasLimitTweak.compute( defaultGas )
    } yield {
      ( effectiveGasPrice, effectiveGas )
    }
  }

  def futureTransactionReceipt(
    transactionHash : EthHash
  )( implicit icontext : Invoker.Context, cfactory : Client.Factory, poller : Poller, econtext : ExecutionContext ) : Future[Option[ClientTransactionReceipt]] = {

    borrow( cfactory( new URL( icontext.jsonRpcUrl ) ) ) { client =>

      def repoll = client.eth.getTransactionReceipt( transactionHash )

      val holder = new AtomicReference[Future[Option[ClientTransactionReceipt]]]( repoll )

      def doPoll() : Option[ClientTransactionReceipt] = {
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

      val task = new Poller.Task( s"Polling for transaction hash '0x${transactionHash.hex}'", icontext.pollPeriod, doPoll, icontext.pollTimeout )

      poller.addTask( task ).map( Some.apply ).recover {
        case e : Poller.TimeoutException => None 
      }
    }
  }

  def awaitTransactionReceipt( transactionHash : EthHash )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Option[ClientTransactionReceipt] = {
    Await.result( futureTransactionReceipt( transactionHash ), Duration.Inf ) // the timeout is enforced within futureTransactionReceipt( ... ), not here
  }

  def requireTransactionReceipt( transactionHash : EthHash, timeout : Duration = Duration.Inf )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : ClientTransactionReceipt = {
    awaitTransactionReceipt( transactionHash ).getOrElse( throw new TimeoutException( transactionHash, timeout ) )
  }


  final object transaction {

    def sendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext ) : Future[EthHash] = {
      sendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte] )
    }

    def sendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, cfactory : Client.Factory, econtext : ExecutionContext ) : Future[EthHash] = {
      borrow( cfactory( icontext.jsonRpcUrl ) ) { client =>

        val from = senderSigner.address

        val fGasPriceGas = gasPriceGas( client, from, to, valueInWei, data )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          ( effectiveGasPrice, effectiveGas ) <- fGasPriceGas
          nextNonce <- fNextNonce
          unsigned = EthTransaction.Unsigned.Message( Unsigned256(nextNonce), Unsigned256(effectiveGasPrice), Unsigned256(effectiveGas), to, valueInWei, data )
          signed = unsigned.sign( senderSigner )
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
      borrow( cfactory( icontext.jsonRpcUrl ) ) { client =>
        val fGasPriceGas = gasPriceGas( client, from, to, valueInWei, data )
        for {
          ( effectiveGasPrice, effectiveGas ) <- fGasPriceGas
          outBytes <- client.eth.call( Some( from ), Some( to ), Some( effectiveGas ), Some( effectiveGasPrice ), Some( valueInWei.widen ), Some( data ) )
        } yield {
          outBytes
        }
      }
    }
  }
}
