/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

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

