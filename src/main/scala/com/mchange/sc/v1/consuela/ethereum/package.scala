package com.mchange.sc.v1.consuela;

import ethereum.encoding.RLPSerializing;

import com.mchange.sc.v1.consuela.hash.Hash;

package object ethereum {
  class EthereumException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class UnexpectedSignatureFormatException( message : String, t : Throwable = null ) extends EthereumException( message, t );

  type EthHash    = Hash.SHA3_256;
  val  EthHash    = Hash.SHA3_256;
  val  EthHashLen = Hash.SHA3_256.HashLength;

  implicit object EthHashSerializer extends RLPSerializing.ByteArrayValue[EthHash]( EthHash.withBytes );

  val EmptyByteSeqHash = EthHash.hash( encoding.RLP.Encoded.EmptyByteSeq )
}
