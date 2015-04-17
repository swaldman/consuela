package com.mchange.sc.v1.consuela.ethereum.encoding;

object HP {
  def encode( nibbles : Seq[Nibble], flag : Boolean ) : Seq[Byte] = {
    require( nibbles.forall( _ < 16 ), s"nibbles should be values between 0 and 15 [ nibbles -> ${ nibbles} ]" );
    _encode( nibbles, flag ).map( _.asInstanceOf[Byte] )
  }
  private[this] def _encode( nibbles : Seq[Nibble], flag : Boolean ) : Seq[Int] = {
    val f = if ( flag ) 2 else 0;
    val len = nibbles.length;
    val even = (len % 2 == 0);
    def combine( start : Int ) : Int = (nibbles( start ) << 4) + (nibbles( start + 1 ));
    def reverseBuild( accum : List[Int], start : Int ) : List[Int] = {
      //println( s"accum: ${accum}" )
      if ( start < len ) reverseBuild( combine( start ) :: accum, start + 2 );
      else accum;
    }
    if ( even ) {
      val headerByte = f << 4;
      reverseBuild( headerByte :: Nil, 0 ).reverse
    } else {
      val headerByte = ((f + 1) << 4) + nibbles(0);
      reverseBuild( headerByte :: Nil, 1 ).reverse
    }
  }
  def decode( bytes : Seq[Byte] ) : ( Seq[Nibble], Boolean ) = _decode( bytes.map( _ & 0xFF ) )
  private[this] def _decode( bytes : Seq[Int] ) : ( Seq[Nibble], Boolean ) = {
    val headerByte = bytes(0);
    val headerNibble = (headerByte >>> 4);
    val flag = ( headerNibble & 2 ) != 0
    val even = ( headerNibble & 1 ) == 0;
    def toNibbles( byte : Int ) : Array[Nibble] = Array( byte >>> 4, byte & 0x0F )
    val nibbles = {
      val nonheader = bytes.drop(1).flatMap( toNibbles(_) );
      if ( even ) (nonheader) else ((headerByte & 0x0F) +: nonheader);
    }
    ( nibbles, flag )
  }
}

