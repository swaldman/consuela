package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Client,Invoker}
import com.mchange.sc.v2.concurrent.{Poller, Scheduler}
import com.mchange.sc.v2.net.{URLSource,LoadBalancer}

object Context {
  final object Default extends Invoker.Context.Default {
    val Scheduler = com.mchange.sc.v2.concurrent.Scheduler.Default
    val EventConfirmations = 12
  }

  def fromUrl[ U : URLSource ](
    jsonRpcUrl         : U,
    gasPriceTweak      : MarkupOrOverride  = Default.GasPriceTweak,
    gasLimitTweak      : MarkupOrOverride  = Default.GasLimitTweak,
    pollPeriod         : Duration          = Default.PollPeriod,
    pollTimeout        : Duration          = Default.PollTimeout,
    gasApprover        : GasApprover       = Default.GasApprover,
    transactionLogger  : TransactionLogger = Default.TransactionLogger,
    eventConfirmations : Int               = Default.EventConfirmations
  )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromUrl (
      jsonRpcUrl        = jsonRpcUrl,
      gasPriceTweak     = gasPriceTweak,
      gasLimitTweak     = gasLimitTweak,
      pollPeriod        = pollPeriod,
      pollTimeout       = pollTimeout,
      gasApprover       = gasApprover,
      transactionLogger = transactionLogger
    )( implicitly[URLSource[U]], cfactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler )
  }
  def fromUrls[ U : URLSource ](
    jsonRpcUrls        : immutable.Iterable[U],
    gasPriceTweak      : MarkupOrOverride  = Default.GasPriceTweak,
    gasLimitTweak      : MarkupOrOverride  = Default.GasLimitTweak,
    pollPeriod         : Duration          = Default.PollPeriod,
    pollTimeout        : Duration          = Default.PollTimeout,
    gasApprover        : GasApprover       = Default.GasApprover,
    transactionLogger  : TransactionLogger = Default.TransactionLogger,
    eventConfirmations : Int               = Default.EventConfirmations
  )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromUrls (
      jsonRpcUrls        = jsonRpcUrls,
      gasPriceTweak      = gasPriceTweak,
      gasLimitTweak      = gasLimitTweak,
      pollPeriod         = pollPeriod,
      pollTimeout        = pollTimeout,
      gasApprover        = gasApprover,
      transactionLogger  = transactionLogger
    )( implicitly[URLSource[U]], cfactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler )
  }
  def fromLoadBalancer (
    loadBalancer       : LoadBalancer,
    gasPriceTweak      : MarkupOrOverride  = Default.GasPriceTweak,
    gasLimitTweak      : MarkupOrOverride  = Default.GasLimitTweak,
    pollPeriod         : Duration          = Default.PollPeriod,
    pollTimeout        : Duration          = Default.PollTimeout,
    gasApprover        : GasApprover       = Default.GasApprover,
    transactionLogger  : TransactionLogger = Default.TransactionLogger,
    eventConfirmations : Int               = Default.EventConfirmations
  )( implicit cfactory : Client.Factory = Default.ClientFactory, poller : Poller = Default.Poller, scheduler : Scheduler = Default.Scheduler, econtext : ExecutionContext = Default.ExecutionContext ) : Context = {
    val icontext = Invoker.Context.fromLoadBalancer (
      loadBalancer       = loadBalancer,
      gasPriceTweak      = gasPriceTweak,
      gasLimitTweak      = gasLimitTweak,
      pollPeriod         = pollPeriod,
      pollTimeout        = pollTimeout,
      gasApprover        = gasApprover,
      transactionLogger  = transactionLogger
    )( cfactory, poller, econtext )
    Context( icontext, eventConfirmations, scheduler )
  }
}
case class Context( icontext : Invoker.Context, eventConfirmations : Int, scheduler : Scheduler )
