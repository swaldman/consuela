package com.mchange.sc.v1.consuela.ethereum.stub

final object Utilities {
  val Zero = sol.UInt256(0)

  def anyIntegralToBigInt( a : Any ) : BigInt = {
    a match {
      case b  : Byte   => BigInt( b )
      case s  : Short  => BigInt( s )
      case i  : Int    => BigInt( i )
      case l  : Long   => BigInt( l )
      case bi : BigInt => bi
      case _           => throw new StubException( s"${a} is not an integral type, cannot be converted to BigInt." )
    }
  }
}
