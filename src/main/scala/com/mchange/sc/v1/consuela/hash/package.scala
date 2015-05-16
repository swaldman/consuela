package com.mchange.sc.v1.consuela;

import java.io.InputStream;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

package object hash {

  val DefaultBufferSize = 1024 * 1024;

  def doHash( algoName : String, is : InputStream, bufferSize : Int = DefaultBufferSize )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance( algoName, provider.name );
    val buffer = new Array[Byte]( bufferSize );
    var count = is.read( buffer );
    while ( count >= 0 ) {
      md.update( buffer, 0, count );
      count = is.read( buffer );
    }
    md.digest()
  }
  def doHash( algoName : String, bytes : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance(algoName, provider.name );
    md.update( bytes, 0, bytes.length );
    md.digest()
  }
  def doHash( algoName : String, bytes : Seq[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = doHash( algoName, bytes.toArray )( provider )
}
