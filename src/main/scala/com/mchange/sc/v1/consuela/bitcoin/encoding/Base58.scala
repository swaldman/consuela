package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.hash.SHA256

import scala.annotation.tailrec
import scala.collection._

object Base58 {
  private final val Alphabet       = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toArray
  private final val AlphabetLength = Alphabet.length

  private final val BigAlphabetLength = BigInt( AlphabetLength )

  private final val IndexForLetter = Alphabet.zip( BigInt(0) until BigAlphabetLength ).toMap

  private final val BigZero = BigInt(0)

  assert( AlphabetLength == 58 )

  private final val LeadingZeroDigitAsString = "" + Alphabet(0)

  private final val CheckLength = 4

  private def indexForLetter( letter : Char ) : BigInt = {
    try {
      IndexForLetter( letter )
    }
    catch {
      case nse : NoSuchElementException => throw new IllegalLetterException( s"Letter '${letter}' does not exist in the Base58 alphabet.", nse )
    }
  }

  def fromBigInt( number : BigInt ) : String = {
    require( number >= 0, s"Only positive integers can be represented in Base58. Can't convert ${number}.")
    _fromBigInt( number )
  }

  def fromByteArray( bytes : Array[Byte] ) : String = {
    val ( leadingZeros, numberBytes ) = bytes.span( _ == 0x00 )
    val digits = if ( numberBytes.isEmpty ) "" else Base58.fromBigInt( numberBytes.toUnsignedBigInt )
    val zeros = (LeadingZeroDigitAsString * leadingZeros.length )
    zeros + digits
  }
  def fromBytes( bytes : Array[Byte] ) : String = fromByteArray( bytes )
  def fromBytes( bytes : immutable.Seq[Byte] ) : String = fromByteArray( bytes.toArray )

  def toByteArray( digits : String ) : Array[Byte] = {
    val ( zeroPart, numPart ) = digits.span( _ == '1' )

    if ( numPart.isEmpty ) {
      Array.ofDim[Byte](zeroPart.length) // initialized to the zeroes we want
    }
    else {
      val number = Base58.toBigInt( numPart )
      val numberBytes = number.toByteArray.dropWhile( _ == 0x00 ) // BigInt bytes will have a zero prepended if sign bit of the desired-to-be-positive byte representation would otherwise be negative
      val raw = Array.ofDim[Byte](zeroPart.length + numberBytes.length)
      Array.copy( numberBytes, 0, raw, zeroPart.length, numberBytes.length ) // bytes prior to zeroPart.length left as 0x00
      raw
    }
  }
  def toBytes( digits : String ) : immutable.Seq[Byte] = toByteArray( digits ).toImmutableSeq

  private def _fromBigInt( number : BigInt ) : String = {
    @tailrec
    def mkdigits( accum : List[Char], reduced : BigInt ) : List[Char] = {
      val ( quotient, remainder ) = reduced /% AlphabetLength
      quotient match {
        case BigZero => Alphabet(remainder.toInt) :: accum
        case _       => mkdigits( Alphabet(remainder.toInt) :: accum, quotient )
      }
    }

    mkdigits( Nil, number ).mkString
  }

  def toBigInt( digits : String ) : BigInt = {
    @tailrec
    def buildNumber( current : BigInt, place : Int, reverseDigits : List[Char] ) : BigInt = {
      reverseDigits match {
        case Nil => {
          assert( current == BigZero && place == 0 )
          BigZero
        }
        case biggest :: Nil => {
          current + ( indexForLetter(biggest) * BigAlphabetLength.pow(place) )
        }
        case next :: more  => {
          buildNumber( current + ( indexForLetter(next) * BigAlphabetLength.pow(place) ), place + 1, more )
        }
      }
    }

    buildNumber( BigZero, 0, digits.toList.reverse )
  }

  def encodeChecked( header : Byte, payload : Array[Byte] ) : String = {
    val headered = {
      val raw = Array.ofDim[Byte]( payload.length + 1 )
      raw(0) = header
      Array.copy( payload, 0, raw, 1, payload.length )
      raw
    }
    val check = doublehash( headered ).take( CheckLength )
    val checked = {
      val raw = Array.ofDim[Byte]( headered.length + CheckLength )
      Array.copy( headered, 0, raw, 0, headered.length )
      Array.copy( check, 0, raw, headered.length, CheckLength )
      raw
    }
    fromBytes( checked )
  }

  def decodeChecked( s : String ) : ( Byte, Array[Byte] ) = {

    val allBytes = toByteArray( s )

    val ( headered, check ) = allBytes.splitAt( allBytes.length - CheckLength )

    val expectedCheck = doublehash( headered ).take( CheckLength )

    if ( java.util.Arrays.equals(check,expectedCheck) ) {
      ( headered(0), headered.tail )
    }
    else {
      throw new Base58ChecksumFailedException( s"Checksum failed! Expected: 0x${expectedCheck.hex}, Found: 0x${check.hex}" )
    }
  }

  private def doublehash( arr : Array[Byte] ) : Array[Byte] = {
    SHA256.hash( SHA256.hash( arr ).toByteArray ).toByteArray
  }
}
