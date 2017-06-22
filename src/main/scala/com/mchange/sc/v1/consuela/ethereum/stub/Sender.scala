package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.wallet
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash, EthKeyPair, EthPrivateKey, EthSigner}

import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256


object Sender {
  final case class WalletV3( w : wallet.V3, passphraseFinder : () => String )( implicit provider : jce.Provider ) extends Sender {

    def address : EthAddress = w.address

    def findSigner() : EthSigner = w.decode( passphraseFinder() )
  }
  final case class Basic( signer : EthSigner ) extends Sender {
    def address = signer.address
    def findSigner = signer
  }

  /**
    * This is just a conventional account to use as an Ether fountain in testing environments
    *
    * Corresponds to eth address 0xaba220742442621625bb1160961d2cfcb64c7682
    */
  lazy val TestSigner = EthPrivateKey( BigInt( 0x7e57 ) )

  lazy val TestSender = Sender.Basic( TestSigner )

  lazy val Default = TestSender
}
trait Sender {
  def address : EthAddress

  def findSigner() : EthSigner

  def sendWei( to : EthAddress, valueInWei : Unsigned256 )(implicit icontext : jsonrpc.Invoker.Context, cfactory : jsonrpc.Client.Factory, econtext : ExecutionContext ) : Future[EthHash] = {
    jsonrpc.Invoker.transaction.sendWei( this.findSigner(), to, valueInWei )
  }
  def sendMessage( to : EthAddress, valueInWei : Unsigned256, data : immutable.Seq[Byte] )(implicit icontext : jsonrpc.Invoker.Context, cfactory : jsonrpc.Client.Factory, econtext : ExecutionContext ) : Future[EthHash] = {
    jsonrpc.Invoker.transaction.sendMessage( this.findSigner(), to, valueInWei, data )
  }
}
