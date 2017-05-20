package com.mchange.sc.v1.consuela.ethereum.ethabi.stub

import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.wallet

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthKeyPair,EthPrivateKey}

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
}
