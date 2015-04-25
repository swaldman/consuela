package com.mchange.sc.v1.consuela;


import com.mchange.sc.v1.log._;
import MLevel._;

package object math {
  implicit lazy val logger = MLogger( this )

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

  def zeroPadLeft( bytes : Array[Byte], desiredLength : Int ) : Array[Byte] = {
    val len = bytes.length;
    require( len <= desiredLength );
    zeroPad( bytes, len, desiredLength );
  }

  private[this] def zeroPad( bytes : Array[Byte], bytesLength : Int, desiredLength : Int ) : Array[Byte] = {
    val out = Array.ofDim[Byte]( desiredLength ); // we're relying on the fact that the default Byte value is zero
    Array.copy( bytes, 0, out, desiredLength - bytesLength, bytesLength )
    out
  }
}
