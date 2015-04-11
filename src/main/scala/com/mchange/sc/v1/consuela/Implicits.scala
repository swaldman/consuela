package com.mchange.sc.v1.consuela;

import com.mchange.lang.ByteUtils;
import scala.language.implicitConversions;

object Implicits {
  implicit val MainProvider : jce.Provider = jce.Provider.ConfiguredProvider;

  implicit class RichByteSeq( bytes : Seq[Byte] ) {
    def hex : String = ByteUtils.toLowercaseHexAscii( bytes.toArray )
  }
  implicit class RichByteArray( bytes : Array[Byte] ) {
    def hex : String = ByteUtils.toLowercaseHexAscii( bytes )
  }
  implicit class RichBigInt( bi : BigInt ) {
    /**
     * Ignores sign and converts the byte representation
     * of the BigInt to the desired len by removing or padding
     * with leading zeros.
     */    
    def unsignedBytes( len : Int ) : Array[Byte] = {
      math.asFixedLengthUnsignedByteArray( bi, len )
    }
  }
  implicit def toRichBigInt( bi : java.math.BigInteger ) = new RichBigInt( bi );
}
