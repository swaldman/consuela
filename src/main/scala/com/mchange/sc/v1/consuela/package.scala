package com.mchange.sc.v1;

import com.mchange.lang.ByteUtils;
import scala.language.implicitConversions;

import java.math.BigInteger;
import javax.xml.bind.DatatypeConverter;

import scala.util.{Try, Success, Failure};

package object consuela {
  class ConsuelaException( message : String, t : Throwable = null ) extends Exception( message, t );
  class UnhandledFailException( fail : Fail ) extends Exception( fail.toString, fail.source match { case th : Throwable => th; case _ => null } );

  implicit val MainProvider : crypto.jce.Provider = crypto.jce.Provider.ConfiguredProvider;

  val lineSeparator = scala.util.Properties.lineSeparator;

  // kind of yuk, but we've renamed this from "Failure" to "Fail" to avoid inconvenient
  // need to qualify names when working with scala.util.Failure.
  case class Fail( message : String, source : Any, mbStackTrace : Option[Array[StackTraceElement]] ) {
    override def toString() : String = mbStackTrace.fold( message ) { stackTrace =>
      (List( message ) ++ stackTrace).mkString( lineSeparator )
    }
    def vomit : Nothing = throw new UnhandledFailException( this );
  }

  trait FailSource[T] {
    def getMessage( source : T ) : String;
    def getStackTrace( source : T ) : Array[StackTraceElement] = Thread.currentThread().getStackTrace();

    def getFail( source : T, includeStackTrace : Boolean = true ) : Fail = {
      val mbStackTrace = if ( includeStackTrace ) Some( getStackTrace( source ) ) else None;
      Fail( getMessage( source ), source, mbStackTrace )
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

  // right-bias Failable[T], for convenience and to render its API more analogous to Option[T]
  implicit class FailableOps[T]( val failable : Failable[T] ) extends AnyVal {
    def get : T = failable match {
      case Left( fail )   => fail.vomit;
      case Right( value ) => value;
    }

    // right-bias the Either, modified from Scala's RightProjection source
    def foreach[U]( f : T => U )                        = failable.right.foreach( f );
    def getOrElse[TT >: T](or : =>TT)                   = failable.right.getOrElse( or );
    def forall( f : T => Boolean )                      = failable.right.forall( f );
    def exists( f : T => Boolean)                       = failable.right.exists( f );
    def flatMap[FF >: Fail, Y]( f: T => Either[FF, Y] ) = failable.right.flatMap( f );
    def map[Y]( f: T => Y )                             = failable.right.map( f );
    def filter( p: T => Boolean ) : Option[Failable[T]] = failable.right.filter( p );
    def toSeq                                           = failable.right.toSeq;
    def toOption                                        = failable.right.toOption;

    //other methods
    def flatten[U](implicit evidence: T <:< Failable[U]) : Failable[U] = {
      failable match {
        case oops @ Left( _ ) => refail( oops );
        case Right( t )   => evidence( t );
      }
    }
  }

  def fail[S : FailSource]( source : S, includeStackTrace : Boolean = true ) : Failable[Nothing] = {
    val ms = implicitly[FailSource[S]];
    val failure = ms.getFail( source, includeStackTrace );
    Left( failure ) : Failable[Nothing];
  }

  /**
   * A utility to re-establish the irrelevant right type as universally acceptable Nothing
   */  
  def refail( prefail : Left[Fail,Any] ) : Failable[Nothing] = prefail.asInstanceOf[Failable[Nothing]]

  def succeed[T]( value : T) : Failable[T] = Right( value );

  val poop : PartialFunction[Throwable, Failable[Nothing]] = { case scala.util.control.NonFatal( t : Throwable ) => fail( t ) }

  implicit class FailableTry[T]( val attempt : Try[T] ) extends AnyVal {
    def toFailable : Failable[T] = attempt match {
      case Success( value )     => succeed( value );
      case Failure( exception ) => fail( exception, true );
    }
  }

  implicit class FailableOption[T]( val maybe : Option[T] ) extends AnyVal {
    def toFailable[ U : FailSource ]( source : U = "No information available." ) : Failable[T] = {
      maybe match {
        case Some( value )  => succeed( value );
        case None           => fail( source, true );
      }
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
