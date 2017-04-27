package com.mchange.sc.v1.consuela.ethereum.ethabi.stub

object Util {
  private val One = BigInt(1)

  private def limitExclusiveUnsigned( bitlength : Int ) = One << bitlength

  private def upperLimitExcusiveSigned( bitlength : Int )  = One << (bitlength - 1)
  private def lowerLimitInclusiveSigned( bitlength : Int ) = -upperLimitExcusiveSigned(bitlength)

  private def isValidSigned(bitlength : Int)( value : BigInt ) : Boolean = {
    val ule = upperLimitExcusiveSigned( bitlength )
    val lli = lowerLimitInclusiveSigned( bitlength )

    value >= lli && value < ule
  }

  private def isValidUnsigned( bitlength : Int )( value : BigInt ) : Boolean = {
    value >= 0 && value < limitExclusiveUnsigned( bitlength )
  }

  def restrictValidSigned(bitlength : Int)( value : BigInt ) : BigInt = {
    if (! isValidSigned(bitlength)(value)) {
      throw new IllegalArgumentException( s"$value exceeds the bounds of a ${bitlength}-bit signed int." )
    } else {
      value
    }
  }
  def restrictValidUnsigned(bitlength : Int)( value : BigInt ) : BigInt = {
    if (! isValidUnsigned(bitlength)(value)) {
      throw new IllegalArgumentException( s"$value exceeds the bounds of a ${bitlength}-bit unsigned int." )
    } else {
      value
    }
  }
  
}
