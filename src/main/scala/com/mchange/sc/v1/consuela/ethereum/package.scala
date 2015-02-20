package com.mchange.sc.v1.consuela;

package object ethereum {
  type Nibble = Int;

  val Nibbles = (0x0 to 0xF).toIndexedSeq

  /**
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */  
  def toNibbles( bytes : Seq[Byte] ) : Seq[Nibble] = lsbNibbles( bytes.map( _ & 0xFF ) )

  /**
   *  Least significant byte nibbles, ignores top three bytes!
   * 
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */ 
  def lsbNibbles( bytes : Seq[Int] ) : Seq[Nibble] = {
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
