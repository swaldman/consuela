package com.mchange.sc.v1.consuela.ethereum

object EthSigners extends EthSigners {
  abstract class PrivateKeySigner( privateKey : EthPrivateKey ) extends EthSigner {
    def sign( document : Array[Byte] ) : EthSignature = privateKey.sign( document )
    def sign( document : Seq[Byte] )   : EthSignature = privateKey.sign( document )

    def address : EthAddress = privateKey.address
  }
}

trait EthSigners {
  import EthSigners.PrivateKeySigner

  implicit class HexStringSigner( hex : String )    extends PrivateKeySigner( EthPrivateKey( hex ) )
  implicit class BigIntSigner( s : String )         extends PrivateKeySigner( EthPrivateKey( s ) )
  implicit class ArraySigner( bytes : Array[Byte] ) extends PrivateKeySigner( EthPrivateKey( bytes ) )
  implicit class SeqSigner( bytes : Seq[Byte] )     extends PrivateKeySigner( EthPrivateKey( bytes ) )
}
