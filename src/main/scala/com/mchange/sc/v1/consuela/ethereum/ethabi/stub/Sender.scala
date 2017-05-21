package com.mchange.sc.v1.consuela.ethereum.ethabi.stub

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.wallet
import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.Invoker

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash, EthKeyPair,EthPrivateKey}

import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256


object Sender {
  final case class WalletV3( w : wallet.V3, passphraseFinder : () => String )( implicit provider : jce.Provider ) extends Sender {

    def address : EthAddress = w.address

    def findPrivateKey() : EthPrivateKey = w.decode( passphraseFinder() )
  }
  final case class Basic( keyPair : EthKeyPair ) extends Sender {
    def address = keyPair.address
    def findPrivateKey = keyPair.pvt
  }
}
trait Sender {
  def address : EthAddress

  def findPrivateKey() : EthPrivateKey

  def sendWei( to : EthAddress, valueInWei : Unsigned256 )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
    Invoker.transaction.sendWei( this.findPrivateKey(), to, valueInWei )
  }
  def sendMessage( to : EthAddress, valueInWei : Unsigned256, data : immutable.Seq[Byte] )(implicit icontext : Invoker.Context, econtext : ExecutionContext ) : Future[EthHash] = {
    Invoker.transaction.sendMessage( this.findPrivateKey(), to, valueInWei, data )
  }
}
