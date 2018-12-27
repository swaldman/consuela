package com.mchange.sc.v1.consuela.ethereum.stub

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

import com.mchange.sc.v1.consuela.crypto.jce
import com.mchange.sc.v1.consuela.ethereum.wallet
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{stub, EthAddress, EthHash, EthKeyPair, EthPrivateKey, EthSigner}

import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned256


object Sender {
  trait Signing extends Sender {
    def findSigner() : EthSigner

    def sendWei( to : EthAddress, valueInWei : Unsigned256 )(implicit scontext : stub.Context ) : Future[EthHash] = {
      jsonrpc.Invoker.transaction.sendWei( this.findSigner(), to, valueInWei )( scontext.icontext )
    }
    def sendMessage( to : EthAddress, valueInWei : Unsigned256, data : immutable.Seq[Byte] )(implicit scontext : stub.Context ) : Future[EthHash] = {
      jsonrpc.Invoker.transaction.sendMessage( this.findSigner(), to, valueInWei, data )( scontext.icontext )
    }
  }
  final case class WalletV3( w : wallet.V3, passphraseFinder : () => String )( implicit provider : jce.Provider ) extends stub.Sender.Signing {

    def address : EthAddress = w.address

    def findSigner() : EthSigner = w.decode( passphraseFinder() )
  }
  final case class Basic( signer : EthSigner ) extends stub.Sender.Signing {
    def address = signer.address
    def findSigner = signer
  }
  final case class View( val address : EthAddress ) extends stub.Sender

  /**
    * This is just a conventional account to use as an Ether fountain in testing environments
    *
    * Corresponds to eth address 0xaba220742442621625bb1160961d2cfcb64c7682
    * with private key 0x7e57 (get it?)
    */
  lazy val TestSender = Test.Sender(0)

  lazy val Default = TestSender
}
trait Sender {
  def address : EthAddress

  def getBalance()(implicit scontext : stub.Context ) : Future[BigInt] = {
    jsonrpc.Invoker.getBalance( address )( scontext.icontext )
  }
  def contractAddress( nonce : BigInt ) : EthAddress = EthAddress.forContract( address, Unsigned256(nonce) )
}
