package com.mchange.sc.v1.consuela.ethereum;

object HP {
  def encode( nibbles : Seq[Int], flag : Boolean ) : Seq[Byte] = {
    require( nibbles.forall( _ < 16 ), s"nibbles should be values between 0 and 15 [ nibbles -> ${ nibbles} ]" );
    _encode( nibbles, flag ).map( _.asInstanceOf[Byte] )
  }
  private[this] def _encode( nibbles : Seq[Int], flag : Boolean ) : Seq[Int] = {
    val f = if ( flag ) 2 else 0;
    val len = nibbles.length;
    val even = (len % 2 == 0);
    def combine( start : Int ) : Int = nibbles( start ) << 4 + nibbles( start + 1 );
    def reverseBuild( accum : List[Int], start : Int ) : List[Int] = {
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
}

