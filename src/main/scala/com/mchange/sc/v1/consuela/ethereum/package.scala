package com.mchange.sc.v1.consuela;

package object ethereum {
  type Nibble = Int;

  def nibbles( bytes : Seq[Byte] ) = lsbNibbles( bytes.map( _ & 0xFF ) )

  /**
   *  Least significant byte nibbles
   */ 
  def lsbNibbles( bytes : Seq[Int] ) : Seq[Int] = {
    val out = new Array[Int]( bytes.length * 2 );
    var idx = 0;
    bytes.foreach {  byte => 
      out(idx) = (byte & 0xF0) >>> 4;
      out(idx+1) = byte & 0x0F;
      idx += 2
    }
    out.toSeq
  }
}
