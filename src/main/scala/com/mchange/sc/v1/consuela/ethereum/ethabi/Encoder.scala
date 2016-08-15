package com.mchange.sc.v1.consuela.ethereum.ethabi

import scala.collection._

import com.mchange.sc.v1.consuela._

import com.mchange.sc.v2.failable._

import com.mchange.lang.ByteUtils.unsignedPromote

import scala.language.existentials

object Encoder {
  private def allZero( bytes : Seq[Byte] ) : Boolean = ! bytes.exists( _ != 0 )

  private val ZeroByte = 0.toByte
  private val OneByte  = 1.toByte

  private val Mappables : Map[String, Encoder[_]] = {
    val fixedLenByteArrayBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( len => s"bytes${len}" -> new Encoder.PredefinedByteArray(len) )
    val byteBinding = ( "byte", fixedLenByteArrayBindings(0)._2 )
    val boolBinding = ( "bool", Encoder.Bool )

    Map( (fixedLenByteArrayBindings :+ byteBinding :+ boolBinding) : _* )
  }


  final object Bool extends Encoder[Boolean] {
    def parse( str : String ) : Failable[Boolean] = {
      Failable( java.lang.Boolean.parseBoolean( str ) )
    }
    def format( representation : Boolean ) : Failable[String] = {
      Failable( String.valueOf( representation ) )
    }
    def encode( representation : Boolean ) : Failable[immutable.Seq[Byte]] = {
      succeed( (0 until 31).map( _ => ZeroByte ) :+ (if ( representation ) OneByte else ZeroByte) )
    }
    def decode( bytes : Seq[Byte] ) : Failable[Boolean] = {
      for {
        _ <- ( bytes.length == 32 ).toFailable( s"bool must be represented as 32 bytes, found ${bytes.length}" )
        _ <- allZero( bytes.init ).toFailable( s"All but the last byte of an encoded bool should be zero! ${bytes.hex}" )
        _ <- ( bytes.tail == 0 || bytes.tail == 1 ).toFailable("The last byte of encoded bool should be 0 or 1.")
      } yield {
        if ( bytes.tail == 0 ) true else false
      }
    }
  }

  final class PredefinedByteArray( len : Int ) extends Encoder[immutable.Seq[Byte]] {
    require( len > 0 && len <= 32 )

    private def checkLen( bytes : Seq[Byte] ) : Failable[Boolean] = (bytes.length == len).toFailable( s"Expected ${len} bytes, found ${bytes.length}: ${bytes}")

    def parse( str : String ) : Failable[immutable.Seq[Byte]] = {
      for {
        compact <- Failable( str.filter( c => !c.isWhitespace ) )
        unwrap  <- (str.length >= 2 && str.head == '[' && str.tail == ']').toFailable("An array should start with '[' and end with ']'.").map( _ => compact.substring(1, compact.length-1) )
        split   <- Failable( unwrap.split(",") )
        check   <- (split.length == len).toFailable( s"A predefined byte array of length ${len} should contain exactly ${len} items, but contains ${split.length}: ${split}" )
        bytes   <- Failable.sequence( split.map( b => Failable( b.toByte ) ) )
      } yield {
        bytes
      }
    }

    def format( representation : immutable.Seq[Byte] ) : Failable[String] = {
      checkLen( representation ).map( _ =>  representation.map( unsignedPromote ).mkString("[",",","]") )
    }

    def encode( representation : immutable.Seq[Byte] ) : Failable[immutable.Seq[Byte]] = {
      val padLength = 32 - len
      val padding = (0 until padLength).map( _ => ZeroByte )

      for {
        check  <- checkLen( representation )
        padded <- Failable( representation ++ padding )
      } yield {
        padded
      }
    }

    def decode( bytes : Seq[Byte] ) : Failable[immutable.Seq[Byte]] = {
      if ( bytes.length != 32 ) {
        fail( "A predefined byte array should be encoded as exactly 32 bytes" )
      } else {
        val ( good, pad ) = bytes.splitAt( len )
        if ( allZero( pad ) ) {
          succeed( good.toImmutableSeq )
        } else {
          fail( s"Expected byte string of length ${len}, span of nozero bytes (from left) is more than that: ${bytes.hex}" )
        }
      }
    }
  }
}
trait Encoder[REP] {
  def parse( str : String )          : Failable[REP]
  def format( representation : REP ) : Failable[String]

  def encode( representation : REP ) : Failable[immutable.Seq[Byte]]
  def decode( bytes : Seq[Byte] )    : Failable[REP]

  def parseEncode( str : String ) : Failable[immutable.Seq[Byte]] = {
    parse( str ).flatMap( encode )
  }
  def decodeFormat( bytes : Seq[Byte] ) : Failable[String] = {
    decode( bytes ).flatMap( format )
  }
}

