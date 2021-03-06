package com.mchange.sc.v1.consuela.ethereum


/**
  * The chain ID stuff implements EIP-155
  * 
  * See https://eips.ethereum.org/EIPS/eip-155
  */

import specification.Types.{ByteSeqExact32, UnsignedBigInt}

import scala.collection._

object EthSigner {
  trait Source[-T] {
    def toEthSigner( t : T ) : EthSigner 
  }
  implicit final object EthSignerIsSource extends EthSigner.Source[EthSigner] {
    def toEthSigner( signer : EthSigner ) = signer
  }
  implicit final object EthPrivateKeyIsSource extends EthSigner.Source[EthPrivateKey] { // variance issues mean that the generic EthSigner version doesn't work.
    def toEthSigner( signer : EthPrivateKey ) = signer
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
  * For now with consuela, the only implementation is EthPrivateKey.
  * 
  * But we define the trait exists so that more elaborate / secure / decoupled
  * signing services could be supported.
  * 
  * ( sbt-ethereum now includes and implementation, SbtEthereumPlugin.CautiousSigner )
  */
trait EthSigner {

  /*
   *  Abstract methods
   */ 
  def signWithoutHashing( bytesToSign : Array[Byte] ) : EthSignature.Basic

  /*
   * Basic methods (with default implementations)
   */ 

  def signWithoutHashing( bytesToSign : Seq[Byte] ) : EthSignature.Basic = signWithoutHashing( bytesToSign.toArray )


  def sign( document : Array[Byte] ) : EthSignature.Basic = signPrehashed( EthHash.hash( document ) )
  def sign( document : Seq[Byte] )   : EthSignature.Basic = signPrehashed( EthHash.hash( document ) )

  def signPrehashed( documentHash : EthHash ) : EthSignature.Basic = signWithoutHashing( documentHash.bytes )


  /*
   *  Implemented variations for EIP-155 chain ID
   */ 
  def signWithoutHashing( document : Array[Byte], chainId : EthChainId )   : EthSignature.WithChainId = EthSignature.WithChainId( signWithoutHashing( document ), chainId )
  final def signWithoutHashing( document : Array[Byte], chainId : BigInt ) : EthSignature.WithChainId = signWithoutHashing( document, EthChainId( UnsignedBigInt( chainId ) ) )
  final def signWithoutHashing( document : Array[Byte], chainId : Long )   : EthSignature.WithChainId = signWithoutHashing( document, chainId )

  def signWithoutHashing( document : Seq[Byte], chainId : EthChainId )   : EthSignature.WithChainId = EthSignature.WithChainId( signWithoutHashing( document ), chainId )
  final def signWithoutHashing( document : Seq[Byte], chainId : BigInt ) : EthSignature.WithChainId = signWithoutHashing( document, EthChainId( UnsignedBigInt( chainId ) ) )
  final def signWithoutHashing( document : Seq[Byte], chainId : Long )   : EthSignature.WithChainId = signWithoutHashing( document, chainId )

  def sign( document : Array[Byte], chainId : EthChainId )   : EthSignature.WithChainId = EthSignature.WithChainId( sign( document ), chainId )
  final def sign( document : Array[Byte], chainId : BigInt ) : EthSignature.WithChainId = sign( document, EthChainId( UnsignedBigInt( chainId ) ) )
  final def sign( document : Array[Byte], chainId : Long )   : EthSignature.WithChainId = sign( document, chainId )

  def sign( document : Seq[Byte], chainId : EthChainId )   : EthSignature.WithChainId = EthSignature.WithChainId( sign( document ), chainId )
  final def sign( document : Seq[Byte], chainId : BigInt ) : EthSignature.WithChainId = sign( document, EthChainId( UnsignedBigInt( chainId ) ) )
  final def sign( document : Seq[Byte], chainId : Long )   : EthSignature.WithChainId = sign( document, chainId )

  def signPrehashed( documentHash : EthHash, chainId : EthChainId )   : EthSignature.WithChainId = EthSignature.WithChainId( signPrehashed( documentHash ), chainId )
  final def signPrehashed( documentHash : EthHash, chainId : BigInt ) : EthSignature.WithChainId = signPrehashed( documentHash, EthChainId( UnsignedBigInt( chainId ) ) )
  final def signPrehashed( documentHash : EthHash, chainId : Long )   : EthSignature.WithChainId = signPrehashed( documentHash, chainId )
  
  def address : EthAddress
}
