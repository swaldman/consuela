package com.mchange.sc.v1.consuela.ethereum.ethabi

import scala.collection._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.math._

import com.mchange.sc.v2.failable._

import com.mchange.lang.ByteUtils.unsignedPromote

import scala.language.existentials

object Encoder {
  private def allZero( bytes : Seq[Byte] ) : Boolean = ! bytes.exists( _ != 0 )

  private val ZeroByte = 0.toByte
  private val OneByte  = 1.toByte

  private val TwoBigInt = BigInt(2)

  private val Mappables : Map[String, Encoder[_]] = {
    val fixedLenByteArrayBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( len => s"bytes${len}" -> new Encoder.PredefinedByteArray(len) )

    val byteBinding : Tuple2[String,Encoder[_]] = ( "byte", fixedLenByteArrayBindings(0)._2 )
    val boolBinding : Tuple2[String,Encoder[_]] = ( "bool", Encoder.Bool )

    val uintBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( _ * 8 ).map( bitLen => s"uint${bitLen}" -> new Encoder.UInt( bitLen ) )
    val  intBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( _ * 8 ).map( bitLen => s"int${bitLen}" -> new Encoder.SInt( bitLen ) )

    val defaultUIntBinding : Tuple2[String,Encoder[_]] = "uint" -> uintBindings.last._2
    val  defaultIntBinding : Tuple2[String,Encoder[_]] = "int"  -> intBindings.last._2

    val allBindings : List[Tuple2[String,Encoder[_]]] = {
      fixedLenByteArrayBindings.toList ::: uintBindings.toList ::: intBindings.toList ::: byteBinding :: boolBinding :: defaultUIntBinding :: defaultIntBinding :: Nil
    }
    Map( allBindings : _* )
  }


  final object Bool extends FixedLengthRepresentation[Boolean]( 32 ) {
    def parse( str : String ) : Failable[Boolean] = {
      Failable( java.lang.Boolean.parseBoolean( str ) )
    }
    def format( representation : Boolean ) : Failable[String] = {
      Failable( String.valueOf( representation ) )
    }
    def encode( representation : Boolean ) : Failable[immutable.Seq[Byte]] = {
      succeed( (0 until 31).map( _ => ZeroByte ) :+ (if ( representation ) OneByte else ZeroByte) )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : Seq[Byte] ) : Failable[Boolean] = {
      for {
        _ <- allZero( bytes.init ).toFailable( s"All but the last byte of an encoded bool should be zero! ${bytes.hex}" )
        _ <- ( bytes.tail == 0 || bytes.tail == 1 ).toFailable("The last byte of encoded bool should be 0 or 1.")
      } yield {
        if ( bytes.tail == 0 ) true else false
      }
    }
  }
  final class UInt( bitLen : Int ) extends FixedLengthRepresentation[BigInt]( 32 ) {
    require( bitLen % 8 == 0 )

    val byteLen = bitLen / 8

    private def checkRange( v : BigInt ) : Failable[Boolean] = (v >= 0 && v < ceiling ).toFailable( s"uint${bitLen} must be greater than or equal to zero and less than ${ceiling}, but is ${v}" )

    val ceiling = TwoBigInt.pow( bitLen )

    def parse( str : String ) : Failable[BigInt] = {
      val v = BigInt( str )
      checkRange(v).map( _ => v )
    }
    def format( representation : BigInt ) : Failable[String] = {
      checkRange( representation ).map( _.toString )
    }
    def encode( representation : BigInt ) : Failable[immutable.Seq[Byte]] = {
      checkRange( representation ).map( _ => asFixedLengthUnsignedByteArray( representation, 32 ).toImmutableSeq )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : Seq[Byte] ) : Failable[BigInt] = {
      val zeroLen = 32 - byteLen
      val ( zeros, good ) = bytes.splitAt( zeroLen )
      val zeroCheck = allZero( zeros ).toFailable( s"We expect a uint${bitLen} to be left-padded with ${zeroLen} zeroes, bytes: ${bytes.hex}" )
      zeroCheck.map( _ => BigInt( 1, good.toArray ) )
    }
  }
  final class SInt( bitLen : Int ) extends FixedLengthRepresentation[BigInt]( 32 ) {
    require( bitLen % 8 == 0 )

    val byteLen = bitLen / 8

    val ceilingExclusive = TwoBigInt.pow( bitLen - 1 )
    val floorInclusive   = -ceilingExclusive

    private def checkRange( v : BigInt ) : Failable[Boolean] = (v >= floorInclusive && v < ceilingExclusive ).toFailable( s"Required ${floorInclusive} <= uint${bitLen} < ${ceilingExclusive}, but is ${v}" )

    def parse( str : String ) : Failable[BigInt] = {
      val v = BigInt( str )
      checkRange(v).map( _ => v )
    }
    def format( representation : BigInt ) : Failable[String] = {
      checkRange( representation ).map( _.toString )
    }
    def encode( representation : BigInt ) : Failable[immutable.Seq[Byte]] = {
      checkRange( representation ).map( _ => asFixedLengthSignedByteArray( representation, 32 ).toImmutableSeq )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : Seq[Byte] ) : Failable[BigInt] = {
      val neg = bytes(0) < 0
      val padLen = 32 - byteLen
      val ( pad, good ) = bytes.splitAt( padLen )

      def allNegOne( bytes : Seq[Byte] ) = ! bytes.exists( _ != -1 )

      val padCheck = ( if (neg) allNegOne( pad ) else allZero( pad ) ).toFailable( s"We expect a int${bitLen} to be left-padded with ${padLen} sign-matched bytes, bytes: ${bytes.hex}" )
      padCheck.map( _ => BigInt( good.toArray ) )
    }
  }
  final class PredefinedByteArray( len : Int ) extends FixedLengthRepresentation[immutable.Seq[Byte]]( 32 ) {
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
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : Seq[Byte] ) : Failable[immutable.Seq[Byte]] = {
      val ( good, pad ) = bytes.splitAt( len )
      if ( allZero( pad ) ) {
        succeed( good.toImmutableSeq )
      } else {
        fail( s"Expected byte string of length ${len}, span of nozero bytes (from left) is more than that: ${bytes.hex}" )
      }
    }
  }
  abstract class FixedLengthRepresentation[REP]( val repLen : Int ) extends Encoder[REP] {
    def decode( bytes : immutable.Seq[Byte] ) : ( Failable[REP], immutable.Seq[Byte] ) = {
      if ( bytes.length >= repLen ) {
        val split = bytes.splitAt( repLen )
        ( decodeCompleteNoLengthCheck( split._1 ), split._2 )
      } else {
        ( fail( s"Insufficient number of bytes, ${repLen} required, found ${bytes.length}." ), immutable.Seq.empty[Byte] )
      }
    }

    def decodeComplete( bytes : Seq[Byte] ) : Failable[REP] = {
      val check = ( bytes.length == repLen ).toFailable( "A predefined byte array should be encoded as exactly ${repLen} bytes, has ${bytes.length}: ${bytes.hex}" )
      check.flatMap( _ => decodeCompleteNoLengthCheck( bytes ) )
    }

    private [Encoder] def decodeCompleteNoLengthCheck( bytes : Seq[Byte] ) : Failable[REP]
  }
}
trait Encoder[REP] {
  def parse( str : String )          : Failable[REP]
  def format( representation : REP ) : Failable[String]

  def encode( representation : REP )        : Failable[immutable.Seq[Byte]]
  def decode( bytes : immutable.Seq[Byte] ) : ( Failable[REP], immutable.Seq[Byte] )

  def decodeComplete( bytes : Seq[Byte] ) : Failable[REP]

  def parseEncode( str : String ) : Failable[immutable.Seq[Byte]] = {
    parse( str ).flatMap( encode )
  }
  def decodeFormat( bytes : immutable.Seq[Byte] ) : ( Failable[String], immutable.Seq[Byte] ) = {
    val tup = decode( bytes )
    ( tup._1.flatMap( format ), tup._2 )
  }
}

