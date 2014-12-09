package com.mchange.sc.v1.consuela;


import com.mchange.sc.v1.log._;
import MLevel._;

package object math {
  implicit val logger = MLogger( this )

  def asFixedLengthUnsignedByteArray( bi : BigInt, desiredLength : Int ) : Array[Byte] = {
    if ( bi.signum < 0 ) {
      WARNING.log( s"Treating negative BigInt ${bi} as an unsigned value in conversion to fixed-length byte array." );
    }
    val bytes = bi.toByteArray;
    val len = bytes.length;
    if ( len == desiredLength ) {
      bytes
    } else {
      val minimized = bytes.dropWhile( _ == 0 );
      val minimizedLength = minimized.length;
      if ( minimizedLength == desiredLength ) {
        minimized
      } else if ( minimizedLength < desiredLength ) {
        zeroPad( minimized, minimizedLength, desiredLength )
      } else {
        throw new IllegalArgumentException( s"BigInt '${bi}' requires a representation larger than the desired length of ${desiredLength}." )
      }
    }
  }

  private[this] def zeroPad( bytes : Array[Byte], bytesLength : Int, desiredLength : Int ) : Array[Byte] = {
    val out = Array.fill[Byte]( desiredLength )(0)
    Array.copy( bytes, 0, out, desiredLength - bytesLength, bytesLength )
    out
  }

  object Implicits {
    implicit class RichBigInt( bi : BigInt ) {
      def asFixedLengthUnsignedByteArray( desiredLength : Int ) : Array[Byte] = math.asFixedLengthUnsignedByteArray( bi, desiredLength )
    }
  }
}
