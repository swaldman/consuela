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

package com.mchange.sc.v1;

import com.mchange.lang.{ByteUtils,IntegerUtils,LongUtils};

import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq
import scala.language.implicitConversions;

import scala.collection.immutable;

import java.math.BigInteger;
import javax.xml.bind.DatatypeConverter;

package object consuela {
  class ConsuelaException( message : String, t : Throwable = null ) extends Exception( message, t );
  class BadConversionException( message : String, t : Throwable = null ) extends ConsuelaException( message, t )

  implicit val MainProvider : crypto.jce.Provider = crypto.jce.Provider.ConfiguredProvider;

  implicit final class RichByte( val byte : Byte ) extends AnyVal {
    def hex : String = ByteUtils.toLowercaseHexAscii( byte ) 
  }
  implicit final class RichString( val string : String ) extends AnyVal {
    def decodeHex( allowPrefix : Boolean ) : Array[Byte] = {
      val hexstring = if ( allowPrefix && string.startsWith( "0x" ) ) string.substring(2) else string;
      ByteUtils.fromHexAscii( hexstring ); // should we switch to the DatatypeConverter implementation of hex encoding/decoding?
    }
    def decodeHex : Array[Byte] = decodeHex( allowPrefix = true )

    def decodeHexAsSeq( allowPrefix : Boolean ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( this.decodeHex( allowPrefix ) );
    def decodeHexAsSeq : immutable.Seq[Byte] = decodeHexAsSeq( allowPrefix = true )

    def decodeBase64 : Array[Byte] = DatatypeConverter.parseBase64Binary( string );
  }
  trait RichBytes { // if we accept code duplication, we can inline this stuff and let the subclasses extend AnyVal. Hmmm....
    protected val _bytes     : Array[Byte];
    def toImmutableSeq       : immutable.Seq[Byte];
    def base64               : String              = DatatypeConverter.printBase64Binary( _bytes )
    def hex                  : String              = ByteUtils.toLowercaseHexAscii( _bytes ); // should we switch to the DatatypeConverter implementation of hex encoding/decoding?
    def toBigInteger         : BigInteger          = new BigInteger( _bytes );
    def toUnsignedBigInteger : BigInteger          = new BigInteger( 1, _bytes );
    def toBigInt             : BigInt              = BigInt( this.toBigInteger );
    def toUnsignedBigInt     : BigInt              = BigInt( this.toUnsignedBigInteger );

    protected def xor( other : Array[Byte] ) : Array[Byte] = {
      require( _bytes.length == other.length, s"We can only xor sequences or arrays of the same length. [_bytes.length: ${_bytes.length}, other.length: ${other.length}]" )
      (0 until _bytes.length).map( i => (_bytes(i) ^ other(i)).toByte ).toArray
    }
    def ^( other : Seq[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( _bytes ^ other.toArray )
  }
  implicit final class RichByteSeq( bytes : Seq[Byte] ) extends RichBytes {
    protected val _bytes = bytes.toArray;
    def toImmutableSeq : immutable.Seq[Byte]           = ImmutableArraySeq.Byte.createNoCopy( _bytes )
    def ^( other : Array[Byte] ) : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( xor( other ) )
  }
  implicit final class RichByteArray( bytes : Array[Byte] ) extends RichBytes {
    protected val _bytes = bytes;
    def toImmutableSeq : immutable.Seq[Byte] = ImmutableArraySeq.Byte( _bytes )
    def ^( other : Array[Byte] ) : Array[Byte] = xor( other )
  }

  implicit final class RichBigInt( val bi : BigInt ) extends AnyVal {
    /**
     * Ignores sign and converts the byte representation
     * of the BigInt to the desired len by removing or padding
     * with leading zeros.
     */    
    def unsignedBytes( len : Int ) : Array[Byte] = {
      math.asFixedLengthUnsignedByteArray( bi, len )
    }
    def toValidLong = {
      if ( bi.isValidLong ) bi.toLong else throw new BadConversionException( s"BigInt bi cannot be converted to Long without truncation" )
    }
    def toValidInt = {
      if ( bi.isValidInt ) bi.toInt else throw new BadConversionException( s"BigInt bi cannot be converted to Int without truncation" )
    }
  }
  implicit def toRichBigInt( bi : BigInteger ) = new RichBigInt( bi );

  implicit final class RichInt( val i : Int ) extends AnyVal {
    def toByteArrayBigEndian : Array[Byte] = IntegerUtils.byteArrayFromInt( i )
    def fillBigEndian( bytes : Array[Byte], offset : Int = 0 ) = IntegerUtils.intIntoByteArray( i, offset, bytes )
  }
}
