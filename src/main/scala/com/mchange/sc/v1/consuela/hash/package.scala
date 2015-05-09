package com.mchange.sc.v1.consuela;

import java.io.InputStream;
import java.io.ByteArrayInputStream;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

import com.mchange.sc.v2.lang.borrow;

package object hash {

  def doHash( algoName : String, is : InputStream )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance(algoName, provider.name );
    val buffer = new Array[Byte](1024);
    var count = is.read( buffer );
    while ( count >= 0 ) {
      md.update( buffer, 0, count );
      count = is.read( buffer );
    }
    md.digest()
  }
  def doHash( algoName : String, bytes : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    borrow ( new ByteArrayInputStream( bytes ) ) { bais => doHash( algoName, bais )( provider ) }
  }
  def doHash( algoName : String, bytes : Seq[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = doHash( algoName, bytes.toArray )( provider )
}
