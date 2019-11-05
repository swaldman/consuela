package com.mchange.sc.v1.consuela

package object bitcoin {
  class BtcException( message : String, t : Throwable = null ) extends ConsuelaException( message, t )

  class UnexpectedScriptPubKeyFormatException( message : String, t : Throwable = null ) extends BtcException( message, t )
  class UnknownPublicKeyFormatException( message : String, t : Throwable = null ) extends BtcException( message, t )
  class UnknownBtcAddressFormatException( message : String, t : Throwable = null ) extends BtcException( message, t )

  val  ByteSeqExact20 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20
  type ByteSeqExact20 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20

  val  ByteSeqExact32 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact32
  type ByteSeqExact32 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact32
  
  val  ByteSeqExact33 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact33
  type ByteSeqExact33 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact33

  val  ByteSeqExact64 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact64
  type ByteSeqExact64 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact64

  val  ByteSeqExact65 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact65
  type ByteSeqExact65 = com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact65
}
