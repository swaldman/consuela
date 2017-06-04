package com.mchange.sc.v1.consuela.ethereum

/**
  * For now, this is just an abstraction over EthPrivateKey.
  * 
  * But it exists so that more elaborate / secure / decoupled
  * signing services could be supported.
  */ 
trait EthSigner {
  def sign( document : Array[Byte] ) : EthSignature
  def sign( document : Seq[Byte] ) : EthSignature
  def address : EthAddress
}
