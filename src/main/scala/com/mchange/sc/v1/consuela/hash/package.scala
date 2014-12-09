package com.mchange.sc.v1.consuela;

import java.io.InputStream;
import java.io.ByteArrayInputStream;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

package object hash {
  private[this] def tested[T]( value : T)( failureMessage : (T) => String = null )( test : (T) => Boolean ) = if ( test(value) ) value else throw new AssertionError( failureMessage( value ) );

  def hash_SHA3_256( is : InputStream )( implicit provider : jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance("SHA3-256", provider.code );
    val buffer = new Array[Byte](1024);
    var count = is.read( buffer );
    while ( count >= 0 ) {
      md.update( buffer, 0, count );
      count = is.read( buffer );
    }
    tested( md.digest() )( array => s"SHA3_256 should yield a 256-bit / 32-byte value! but length is ${array.length}" )( array => array.length == 32 );
  }
  def hash_SHA3_256( bytes : Array[Byte] )( implicit provider : jce.Provider ) : Array[Byte] = hash_SHA3_256( new ByteArrayInputStream( bytes ) )( provider )
  def hash_SHA3_256( bytes : Seq[Byte] )( implicit provider : jce.Provider ) : Array[Byte] = hash_SHA3_256( bytes.toArray );
}
