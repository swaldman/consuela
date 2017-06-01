package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash, EthSigner, EthTransaction, EthereumException}
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v1.log.MLevel._

import java.net.URL

import scala.annotation.tailrec

import scala.collection._

import scala.concurrent.{Await,ExecutionContext,Future,blocking}
import scala.concurrent.duration._

object Invoker {

  private lazy implicit val logger = mlogger( this )

  class InvokerException( message : String, cause : Throwable = null ) extends EthereumException( message, cause )
  final class TimeoutException( transactionHash : EthHash, timeout : Duration ) extends InvokerException( s"Could not retrieve receipt for transaction '0x${transactionHash.hex}' within ${timeout}" )

  private def rounded( bd : BigDecimal ) = bd.round( bd.mc ) // work around absence of default rounded method in scala 2.10 BigDecimal

  final object MarkupOrOverride {
    def createMarkup( fraction : Double ) = Markup( fraction ) // we use createXXX to avoid using the bare keyword 'override' :(
    def createOverride( value : BigInt ) = Override( value ) 
  }
  sealed trait MarkupOrOverride {
    def compute( default : BigInt ) : BigInt
  }
  final case class Markup( fraction : Double, mbFloorInWei : Option[BigInt] = None, mbCapInWei : Option[BigInt] = None ) extends MarkupOrOverride {
    def compute( default : BigInt ) : BigInt = {
      val base             = rounded( BigDecimal( default ) * BigDecimal(1 + fraction) ).toBigInt
      val floored          = mbFloorInWei.fold( base )( floorInWei => base max floorInWei )
      val flooredAndCapped = mbCapInWei.fold( floored )( capInWei => floored min capInWei )
      flooredAndCapped
    }
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

  def futureTransactionReceipt( transactionHash : EthHash, timeout : Duration )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[Option[ClientTransactionReceipt]] = {
    val pollPeriodMillis = icontext.pollPeriod.toMillis
    val timeoutMillis = if ( timeout == Duration.Inf ) Long.MaxValue else System.currentTimeMillis + timeout.toMillis

    borrow( new Client.Simple( new URL( icontext.jsonRpcUrl ) ) ) { client =>

      def onePoll = client.eth.getTransactionReceipt( transactionHash )

      // this method is not tail recursive... but it isn't actually recursive, as each call to Future.flatMap(...)
      // runs as its own task dispatched to a Thread pool. i wonder whether, after waiting a very, very long, it might
      // consume a lot of memory. but for now i think it is okay.
      //
      // see https://stackoverflow.com/questions/44146832/tail-recursive-functional-polling-in-scala

      def poll( last : Future[Option[ClientTransactionReceipt]] ) : Future[Option[ClientTransactionReceipt]] = {
        last.flatMap { mbReceipt =>
          mbReceipt match {
            case Some( _ ) => last // cool, we got it :)
            case None      => {    // it wasn't available when we checked :(
              if (System.currentTimeMillis > timeoutMillis ) { // we give up
                last
              } else {                                         // try again!
                blocking {
                  Thread.sleep( pollPeriodMillis )
                }
                poll( onePoll )
              }
            }
          }
        }
      }

      poll( onePoll )
    }
  }

  def awaitTransactionReceipt( transactionHash : EthHash, timeout : Duration )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Option[ClientTransactionReceipt] = {
      Await.result( futureTransactionReceipt( transactionHash, timeout ), Duration.Inf ) // the timeout in enforced within futureTransactionReceipt( ... ), not here
  }

  def requireTransactionReceipt( transactionHash : EthHash, timeout : Duration = Duration.Inf )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) : ClientTransactionReceipt = {
    awaitTransactionReceipt( transactionHash, timeout ).getOrElse( throw new TimeoutException( transactionHash, timeout ) )
  }


  final object transaction {

    def sendWei(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256
    )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
      sendMessage( senderSigner, to, valueInWei, immutable.Seq.empty[Byte] )
    }

    def sendMessage(
      senderSigner  : EthSigner,
      to            : EthAddress,
      valueInWei    : Unsigned256,
      data          : immutable.Seq[Byte]
    )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
      borrow( new Client.Simple( new URL( icontext.jsonRpcUrl ) ) ) { client =>

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
