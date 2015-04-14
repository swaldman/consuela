package com.mchange.sc.v1.consuela;

import com.mchange.sc.v1.consuela.hash.Hash;

package object ethereum {
  class EthereumException( message : String, t : Throwable = null ) extends ConsuelaException( message, t );
  class UnexpectedSignatureFormatException( message : String, t : Throwable = null ) extends EthereumException( message, t );

  type EthHash    = Hash.SHA3_256;
  val  EthHash    = Hash.SHA3_256;
  val  EthHashLen = Hash.SHA3_256.HashLength;

  object Unsigned256 {
    val MAX_VALUE_EXCLUSIVE = BigInt(2).pow(256);
    def elem_: ( bi : BigInt ) : Boolean= bi >= 0 && bi < MAX_VALUE_EXCLUSIVE;
  }

  /*
   * 
   * Nibble stuff
   * 
   */ 
  type Nibble = Int;

  val Nibbles = (0x0 to 0xF).toIndexedSeq

  def isNibble( mbNibble : Int ) = ( mbNibble >= 0x0 && mbNibble <= 0xF )

  /**
   *  @return an always-even sequence of Ints between 0x0 and 0xF
   */  
  def toNibbles( bytes : Seq[Byte] ) : IndexedSeq[Nibble] = leastSignificantByteNibbles( bytes.map( _ & 0xFF ) )

  def toNibbles( key : String, charsetStr : String ) : IndexedSeq[Nibble] = toNibbles( key.getBytes( charsetStr ) );

  def toNibbles( key : String ) : IndexedSeq[Nibble] = toNibbles( key, "UTF-8" );

  def nibblesToBytes( nibbles : Seq[Nibble] ) : Seq[Byte] = {
    def bytify( pairAsSeq : Seq[Nibble] ) : Byte = ( (pairAsSeq.head << 4) | (pairAsSeq.last << 0) ).toByte;
    require( nibbles.length % 2 == 0, s"Only an even number of nibbles can be concatenated into bytes! nibbles.length -> ${nibbles.length}" );
    nibbles.sliding(2,2).map( bytify _ ).toSeq;
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
