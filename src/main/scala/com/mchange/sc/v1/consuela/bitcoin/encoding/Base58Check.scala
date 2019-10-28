package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.hash.SHA256

import scala.annotation.tailrec
import scala.collection._

object Base58Check {
  private final val Alphabet       = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz".toArray
  private final val AlphabetLength = Alphabet.length

  private final val BigAlphabetLength = BigInt( AlphabetLength )

  private final val IndexForLetter = Alphabet.zip( BigInt(0) until BigAlphabetLength ).toMap

  private final val BigZero = BigInt(0)

  assert( AlphabetLength == 58 )

  private final val LeadingZeroDigitAsString = "" + Alphabet(0)

  private final val CheckLength = 4

  def encode( header : Byte, payload : Array[Byte] ) : String = {
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
    val ( leadingZeros, numberBytes ) = checked.span( _ == 0x00 )
    val digits : String = {
      if ( numberBytes.length == 0 ) {
        ""
      }
      else {
        val number = BigInt( 1, numberBytes )

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
    }
    val zeros = (LeadingZeroDigitAsString * leadingZeros.length )
    zeros + digits
  }

  def decode( s : String ) : ( Byte, Array[Byte] ) = {
    val ( zeroPart, numPart ) = s.span( _ == '1' )

    @tailrec
    def buildNumber( current : BigInt, place : Int, reverseDigits : List[Char] ) : BigInt = {
      reverseDigits match {
        case Nil => {
          assert( current == BigZero && place == 0 )
          BigZero
        }
        case biggest :: Nil => {
          current + ( IndexForLetter(biggest) * BigAlphabetLength.pow(place) )
        }
        case next :: more  => {
          buildNumber( current + ( IndexForLetter(next) * BigAlphabetLength.pow(place) ), place + 1, more )
        }
      }
    }

    val number = buildNumber( BigZero, 0, numPart.toList.reverse )
    val numberBytes = number.toByteArray
    val allBytes = {
      val raw = Array.ofDim[Byte](zeroPart.length + numberBytes.length)
      Array.copy( numberBytes, 0, raw, zeroPart.length, numberBytes.length )
      raw
    }
    val ( headered, check ) = allBytes.splitAt( allBytes.length - CheckLength )

    val expectedCheck = doublehash( headered ).take( CheckLength )

    if ( java.util.Arrays.equals(check,expectedCheck) ) {
      ( headered(0), headered.tail )
    }
    else {
      throw new NumberFormatException( s"Checksum failed! Expected: 0x${expectedCheck.hex}, Found: 0x${check.hex}" )
    }
  }

  private def doublehash( arr : Array[Byte] ) : Array[Byte] = {
    SHA256.hash( SHA256.hash( arr ).toByteArray ).toByteArray
  }
}
