package com.mchange.sc.v1.consuela.ethereum.pow;

import com.mchange.sc.v1.consuela.ethereum.{EthBlock,EthHash};
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{Unsigned64,Unsigned256};

object ProofOfWork {
  def apply( putativeNonce : Unsigned64, ignoreMixNonceHeader : EthBlock.Header ) : ProofOfWork = {
    val hashimoto = Ethash23Manager.hashimoto( ignoreMixNonceHeader, putativeNonce );
    this.apply( EthHash.withBytes( hashimoto.mixDigest ), hashimoto.result )
  }
  def apply( fullHeader : EthBlock.Header ) : ProofOfWork = apply( fullHeader.nonce, fullHeader );

  def validate( proof : ProofOfWork, fullHeader : EthBlock.Header ) = {
    proof.m == fullHeader.mixHash && proof.n.widen <= (ThresholdNumerator / fullHeader.difficulty.widen )
  }
  def validate( fullHeader : EthBlock.Header ) : Boolean = validate( this.apply( fullHeader ), fullHeader );
}
final case class ProofOfWork private ( m : EthHash, n : Unsigned256 );


