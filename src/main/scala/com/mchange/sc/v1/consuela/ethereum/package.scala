package com.mchange.sc.v1.consuela;

package object ethereum {

  type Nibble = Int;

  val Nibbles = (0x0 to 0xF).toIndexedSeq

  def isNibble( mbNibble : Int ) = ( mbNibble >= 0x0 && mbNibble <= 0xF )

  /**
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */  
  def toNibbles( bytes : Seq[Byte] ) : IndexedSeq[Nibble] = leastSignificantByteNibbles( bytes.map( _ & 0xFF ) )

  def toNibbles( key : String, charsetStr : String ) : IndexedSeq[Nibble] = toNibbles( key.getBytes( charsetStr ) );

  def toNibbles( key : String ) : IndexedSeq[Nibble] = toNibbles( key, "UTF-8" );

  def nibbleToBytes( nibbles : Seq[Nibble] ) : Seq[Byte] = {
    def bytify( pairAsSeq : Seq[Nibble] ) : Byte = ( (pairAsSeq.head << 4) | (pairAsSeq.last << 0) ).toByte;
    require( nibbles.length % 2 == 0, s"Only an even number of nibbles can be concatenated into bytes! nibbles.length -> ${nibbles.length}" );
    nibbles.sliding(2).map( bytify _ ).toSeq;
  }

  /**
   *  Least significant byte nibbles, ignores top three bytes!
   * 
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */ 
  def leastSignificantByteNibbles( bytes : Seq[Int] ) : IndexedSeq[Nibble] = {
    val out = new Array[Int]( bytes.length * 2 );
    var idx = 0;
    bytes.foreach {  byte => 
      out(idx) = (byte & 0xF0) >>> 4;
      out(idx+1) = byte & 0x0F;
      idx += 2
    }
    out.toIndexedSeq
  }

}
