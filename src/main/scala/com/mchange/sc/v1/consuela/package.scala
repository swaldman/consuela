package com.mchange.sc.v1;

import com.mchange.lang.ByteUtils;
import scala.language.implicitConversions;

import java.math.BigInteger;
import javax.xml.bind.DatatypeConverter;

package object consuela {
  class ConsuelaException( message : String, t : Throwable = null ) extends Exception( message, t );

  implicit val MainProvider : crypto.jce.Provider = crypto.jce.Provider.ConfiguredProvider;

  trait MessageSource[T] {
    def getMessage( source : T ) : String;
  }

  object StringAsMessageSource extends MessageSource[String] {
    def getMessage( source : String ) : String = source;
  }

  object ThrowableAsMessageSource extends MessageSource[Throwable] {
    def getMessage( source : Throwable ) : String = source.getMessage();
  }

  type Failable[T] = Either[String,T];

  def fail[T : MessageSource]( source : T ) : Failable[T] = Left( implicitly[MessageSource[T]].getMessage( source ) );
  def succeed[T]( value : T) : Failable[T]                = Right( value );

  implicit class RichString( val string : String ) extends AnyVal {
    def decodeHex : Array[Byte] = {
      val hexstring = if ( string.startsWith( "0x" ) ) string.substring(2) else string;
      ByteUtils.fromHexAscii( hexstring ); // should we switch to the DatatypeConverter implementation of hex encoding/decoding?
    }
    def decodeBase64 : Array[Byte] = DatatypeConverter.parseBase64Binary( string );
  }
  trait RichBytes { // if we accept code duplication, we can inline this stuff and let the subclasses extend AnyVal. Hmmm....
    protected val _bytes     : Array[Byte];
    def base64               : String     = DatatypeConverter.printBase64Binary( _bytes )
    def hex                  : String     = ByteUtils.toLowercaseHexAscii( _bytes ); // should we switch to the DatatypeConverter implementation of hex encoding/decoding?
    def toBigInteger         : BigInteger = new BigInteger( _bytes );
    def toUnsignedBigInteger : BigInteger = new BigInteger( 1, _bytes );
    def toBigInt             : BigInt     = BigInt( this.toBigInteger );
    def toUnsignedBigInt     : BigInt     = BigInt( this.toUnsignedBigInteger );
  }
  implicit class RichByteSeq( bytes : Seq[Byte] ) extends RichBytes {
    protected val _bytes = bytes.toArray;
  }
  implicit class RichByteArray( bytes : Array[Byte] ) extends RichBytes {
    protected val _bytes = bytes;
  }
  implicit class RichBigInt( val bi : BigInt ) extends AnyVal {
    /**
     * Ignores sign and converts the byte representation
     * of the BigInt to the desired len by removing or padding
     * with leading zeros.
     */    
    def unsignedBytes( len : Int ) : Array[Byte] = {
      math.asFixedLengthUnsignedByteArray( bi, len )
    }
  }
  implicit def toRichBigInt( bi : BigInteger ) = new RichBigInt( bi );
}
