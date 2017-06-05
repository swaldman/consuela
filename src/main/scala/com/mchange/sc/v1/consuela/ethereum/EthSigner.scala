package com.mchange.sc.v1.consuela.ethereum

import specification.Types.ByteSeqExact32

object EthSigner {
  trait Source[T] {
    def toEthSigner( t : T ) : EthSigner 
  }
  implicit final object EthSignerIsSource extends EthSigner.Source[EthSigner] {
    def toEthSigner( signer : EthSigner ) = signer
  }
  implicit final object StringIsSource extends EthSigner.Source[String] {
    def toEthSigner( hex : String ) = EthPrivateKey( hex )
  }
  implicit final object ByteSeqIsSource extends EthSigner.Source[Seq[Byte]] {
    def toEthSigner( bytes : Seq[Byte] ) = EthPrivateKey( bytes )
  }
  implicit final object ByteArrayIsSource extends EthSigner.Source[Array[Byte]] {
    def toEthSigner( bytes : Array[Byte] ) = EthPrivateKey( bytes )
  }
  implicit final object BigIntIsSource extends EthSigner.Source[BigInt] {
    def toEthSigner( s : BigInt ) = EthPrivateKey( s )
  }
  implicit final object ByteSeqExact32IsSource extends EthSigner.Source[ByteSeqExact32] {
    def toEthSigner( bytes : ByteSeqExact32 ) = EthPrivateKey( bytes )
  }
}
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
