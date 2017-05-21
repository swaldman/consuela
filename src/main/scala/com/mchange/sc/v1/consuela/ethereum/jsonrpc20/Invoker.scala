package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthPrivateKey, EthHash, EthTransaction, EthereumException}
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v1.log.MLevel._

import java.net.URL

import scala.annotation.tailrec

import scala.collection._

import scala.concurrent.{Await,ExecutionContext,Future}
import scala.concurrent.duration._

object Invoker {

  private lazy implicit val logger = mlogger( this )

  class InvokerException( message : String, cause : Throwable = null ) extends EthereumException( message, cause )
  final class TimeoutException( transactionHash : EthHash, timeout : Duration ) extends InvokerException( s"Could not retrieve receipt for transaction '0x${transactionHash.hex}' within ${timeout}" )

  def rounded( bd : BigDecimal ) = bd.round( bd.mc ) // work around absence of default rounded method in scala 2.10 BigDecimal

  final object MarkupOrOverride {
    def default = Markup( 0 )
    def createMarkup( fraction : Double ) = Markup( fraction ) // we use createXXX to avoid using the bare keyword override :(
    def createOverride( value : BigInt ) = Override( value ) 
  }
  sealed trait MarkupOrOverride {
    def compute( default : BigInt ) : BigInt
  }
  final case class Markup( fraction : Double ) extends MarkupOrOverride {
    def compute( default : BigInt ) : BigInt = rounded( BigDecimal( default ) * BigDecimal(1 + fraction) ).toBigInt
  }
  final case class Override( value : BigInt ) extends MarkupOrOverride {
    def compute( default : BigInt ) : BigInt = value
  }

  final case class Context( jsonRpcUrl : String, gasHandler : MarkupOrOverride = Markup(0.2), gasPriceHandler : MarkupOrOverride = Markup(0), pollPeriod : Duration = 5.seconds )

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
      effectiveGasPrice = icontext.gasPriceHandler.compute( defaultGasPrice )
      defaultGas <- fDefaultGas
      effectiveGas = icontext.gasHandler.compute( defaultGas )
    } yield {
      ( effectiveGasPrice, effectiveGas )
    }
  }

  def awaitTransactionReceipt( transactionHash : EthHash, timeout : Duration )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Option[ClientTransactionReceipt] = {
    val pollPeriodMillis = icontext.pollPeriod.toMillis
    val timeoutMillis = if ( timeout == Duration.Inf ) Long.MaxValue else System.currentTimeMillis + timeout.toMillis

    borrow( new Client.Simple( new URL( icontext.jsonRpcUrl ) ) ) { client =>

      def onePoll = Await.result( client.eth.getTransactionReceipt( transactionHash ), Duration.Inf )

      @tailrec
      def poll( last : Option[ClientTransactionReceipt] ) : Option[ClientTransactionReceipt] = {
        last match {
          case Some( _ ) => last
          case None      => {
            if (System.currentTimeMillis > timeoutMillis ) {
              None
            } else {
              Thread.sleep( pollPeriodMillis )
              poll( onePoll )
            }
          }
        }
      }

      poll( onePoll )
    }
  }

  def requireTransactionReceipt( transactionHash : EthHash, timeout : Duration = Duration.Inf )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : ClientTransactionReceipt = {
    awaitTransactionReceipt( transactionHash, timeout ).getOrElse( throw new TimeoutException( transactionHash, timeout ) )
  }

  final object transaction {

    def sendWei(
      senderKey  : EthPrivateKey,
      to         : EthAddress,
      valueInWei : Unsigned256
    )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
      sendMessage( senderKey, to, valueInWei, immutable.Seq.empty[Byte] )
    }

    def sendMessage(
      senderKey  : EthPrivateKey,
      to         : EthAddress,
      valueInWei : Unsigned256,
      data       : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
      borrow( new Client.Simple( new URL( icontext.jsonRpcUrl ) ) ) { client =>

        val from = senderKey.toPublicKey.toAddress

        val fGasPriceGas = gasPriceGas( client, from, to, valueInWei, data )
        val fNextNonce = client.eth.getTransactionCount( from, Client.BlockNumber.Pending )

        for {
          ( effectiveGasPrice, effectiveGas ) <- fGasPriceGas
          nextNonce <- fNextNonce
          unsigned = EthTransaction.Unsigned.Message( Unsigned256(nextNonce), Unsigned256(effectiveGasPrice), Unsigned256(effectiveGas), to, valueInWei, data )
          signed = unsigned.sign( senderKey )
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
    )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[immutable.Seq[Byte]] = {
      borrow( new Client.Simple( new URL( icontext.jsonRpcUrl ) ) ) { client =>
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
