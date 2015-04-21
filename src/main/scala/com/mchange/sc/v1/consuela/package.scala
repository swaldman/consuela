package com.mchange.sc.v1;

import com.mchange.lang.ByteUtils;
import scala.language.implicitConversions;

import java.math.BigInteger;
import javax.xml.bind.DatatypeConverter;

import scala.util.{Try, Success, Failure};

package object consuela {
  class ConsuelaException( message : String, t : Throwable = null ) extends Exception( message, t );

  implicit val MainProvider : crypto.jce.Provider = crypto.jce.Provider.ConfiguredProvider;

  val lineSeparator = scala.util.Properties.lineSeparator;

  case class Fail( message : String, mbStackTrace : Option[Array[StackTraceElement]] ) {
    override def toString() : String = mbStackTrace.fold( message ) { stackTrace =>
      (List( message ) ++ stackTrace).mkString( lineSeparator )
    }
  }

  trait FailSource[T] {
    def getMessage( source : T ) : String;
    def getStackTrace( source : T ) : Array[StackTraceElement] = Thread.currentThread().getStackTrace();

    def getFail( source : T, includeStackTrace : Boolean = true ) : Fail = {
      val mbStackTrace = if ( includeStackTrace ) Some( getStackTrace( source ) ) else None;
      Fail( getMessage( source ), mbStackTrace )
    }
  }

  implicit object StringAsFailSource extends FailSource[String] {
    def getMessage( source : String ) : String = source;
  }

  implicit object ThrowableAsFailSource extends FailSource[Throwable] {
    def getMessage( source : Throwable ) : String = s"${source.getClass.getName}: ${source.getMessage()}";

    override def getStackTrace( source : Throwable ) = source.getStackTrace;
  }

  type Failable[+T] = Either[Fail,T];

  def fail[S : FailSource]( source : S, includeStackTrace : Boolean = true ) : Failable[Nothing] = {
    val ms = implicitly[FailSource[S]];
    val failure = ms.getFail( source, includeStackTrace );
    Left( failure ) : Failable[Nothing];
  }

  def succeed[T]( value : T) : Failable[T] = Right( value );

  implicit class FailableTry[T]( val attempt : Try[T] ) extends AnyVal {
    def toFailable : Failable[T] = attempt match {
      case Success( value )     => succeed( value );
      case Failure( exception ) => fail( exception, true );
    }
  }

  implicit class FailableOption[T]( val maybe : Option[T] ) extends AnyVal {
    def toFailable : Failable[T] = maybe match {
      case Some( value )  => succeed( value );
      case None           => fail( "No information available.", true );
    }
  }

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
