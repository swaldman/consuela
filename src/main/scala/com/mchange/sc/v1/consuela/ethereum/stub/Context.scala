package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import scala.util.Try

import com.mchange.sc.v1.consuela.ethereum.{EthChainId,EthHash}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Client,Invoker}
import com.mchange.sc.v2.jsonrpc.Exchanger
import com.mchange.sc.v2.concurrent.{Poller, Scheduler}
import com.mchange.sc.v2.net.{URLSource,LoadBalancer}

object Context {
  final object Default extends Invoker.Context.Default {
    val Scheduler = com.mchange.sc.v2.concurrent.Scheduler.Default
    val EventConfirmations = 12
    val OnTransactionSubmitted : Try[EthHash] => Unit = _ => ()
  }

  def fromUrl[ U : URLSource ](
    jsonRpcUrl          : U,
    chainId             : Option[EthChainId]      = Default.ChainId,
    gasPriceTweak       : MarkupOrOverride        = Default.GasPriceTweak,
    gasLimitTweak       : MarkupOrOverride        = Default.GasLimitTweak,
    pollPeriod          : Duration                = Default.PollPeriod,
    pollTimeout         : Duration                = Default.PollTimeout,
    httpTimeout         : Duration                = Default.HttpTimeout,
    transactionApprover : TransactionApprover     = Default.TransactionApprover,
    transactionLogger   : TransactionLogger       = Default.TransactionLogger,
    eventConfirmations  : Int                     = Default.EventConfirmations,
    onTransactionSubmitted : Try[EthHash] => Unit = Default.OnTransactionSubmitted
  )( implicit efactory  : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromUrl (
      jsonRpcUrl          = jsonRpcUrl,
      chainId             = chainId,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,
      httpTimeout         = httpTimeout,
      transactionApprover = transactionApprover,
      transactionLogger   = transactionLogger
    )( implicitly[URLSource[U]], efactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler, onTransactionSubmitted )
  }
  def fromUrls[ U : URLSource ](
    jsonRpcUrls         : immutable.Iterable[U],
    chainId             : Option[EthChainId]      = Default.ChainId,
    gasPriceTweak       : MarkupOrOverride        = Default.GasPriceTweak,
    gasLimitTweak       : MarkupOrOverride        = Default.GasLimitTweak,
    pollPeriod          : Duration                = Default.PollPeriod,
    pollTimeout         : Duration                = Default.PollTimeout,
    httpTimeout         : Duration                = Default.HttpTimeout,
    transactionApprover : TransactionApprover     = Default.TransactionApprover,
    transactionLogger   : TransactionLogger       = Default.TransactionLogger,
    eventConfirmations  : Int                     = Default.EventConfirmations,
    onTransactionSubmitted : Try[EthHash] => Unit = Default.OnTransactionSubmitted
  )( implicit efactory : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromUrls (
      jsonRpcUrls         = jsonRpcUrls,
      chainId             = chainId,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,
      httpTimeout         = httpTimeout,
      transactionApprover = transactionApprover,
      transactionLogger   = transactionLogger
    )( implicitly[URLSource[U]], efactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler, onTransactionSubmitted )
  }
  def fromLoadBalancer (
    loadBalancer        : LoadBalancer,
    chainId             : Option[EthChainId]      = Default.ChainId,
    gasPriceTweak       : MarkupOrOverride        = Default.GasPriceTweak,
    gasLimitTweak       : MarkupOrOverride        = Default.GasLimitTweak,
    pollPeriod          : Duration                = Default.PollPeriod,
    pollTimeout         : Duration                = Default.PollTimeout,
    httpTimeout         : Duration                = Default.HttpTimeout,
    transactionApprover : TransactionApprover     = Default.TransactionApprover,
    transactionLogger   : TransactionLogger       = Default.TransactionLogger,
    eventConfirmations  : Int                     = Default.EventConfirmations,
    onTransactionSubmitted : Try[EthHash] => Unit = Default.OnTransactionSubmitted
  )( implicit efactory : Exchanger.Factory = Default.ExchangerFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromLoadBalancer (
      loadBalancer        = loadBalancer,
      chainId             = chainId,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,
      httpTimeout         = httpTimeout,
      transactionApprover = transactionApprover,
      transactionLogger   = transactionLogger
    )( efactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler, onTransactionSubmitted )
  }
}
case class Context( icontext : Invoker.Context, eventConfirmations : Int, scheduler : Scheduler, onTransactionSubmitted : Try[EthHash] => Unit )
