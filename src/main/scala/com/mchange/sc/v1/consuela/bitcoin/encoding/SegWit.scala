package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._

import scala.annotation.tailrec
import scala.collection._

final object SegWit {
  private final val Mask5           = (1 << 5) - 1
  private final val UpperMask5      = (Mask5 << 5)
  private final val UnoffsetWindow5 = (Mask5 << 11)
  private final val UnoffsetWindow8 = (0xFF  << 7)

  def decode( expectedHrp : Option[String], encoded : String ) : ( Byte, immutable.Seq[Byte] ) = {
    val ( v, w ) = decodeAsArray( expectedHrp, encoded )
    ( v, w.toImmutableSeq )
  }

  def decodeAsArray( expectedHrp : Option[String], encoded : String ) : ( Byte, Array[Byte] ) = {
    val quintets = Bech32.decodeQuintets( expectedHrp, encoded )
    if (quintets.nonEmpty) {
      val out = ( quintets.head.toByte, packQuintets( quintets.tail.toArray ) )
      ensureValidVersionWitnessProgramPairTupled(out)
      out
    }
    else {
      throw new InvalidSegWitException( s"A valid SegWit address must contain data. The (valid) Bech32 encoding '${encoded}' does not." )
    }
  }
  def encode( hrp : String, version : Byte, witnessProgram : Array[Byte] ) : String = {

    ensureValidVersionWitnessProgramPair( version, witnessProgram )

    val quintets = version.toInt :: expandToQuintets( witnessProgram )
    Bech32.encodeQuintets( hrp, quintets.toArray )
  }

  def encode( hrp : String, version : Byte, witnessProgram : immutable.Seq[Byte] ) : String = {
    encode( hrp, version, witnessProgram.toArray )
  }

  private def ensureValidVersionWitnessProgramPair( version : Byte, witnessProgram : Array[Byte] ) : Unit = {
    ( version, witnessProgram.length ) match {
      case ( 0, 20 )   => ()
      case ( 0, 32 )   => ()
      case ( 0, len  ) => throw new InvalidSegWitException( s"Version 0x0 addresses must have 20 or 32 byte witness programs, found ${len} bytes: 0x${witnessProgram.hex}" )
      case _           => ()
    }
  }

  private val ensureValidVersionWitnessProgramPairTupled = (ensureValidVersionWitnessProgramPair _).tupled

  private def expandToQuintets( witnessProgram : Array[Byte] ) : List[Int] = {
    if ( witnessProgram.nonEmpty ) {

      val len = witnessProgram.length

      def encode( startByte : Int, startBit : Int, reverseQuintetAccum : List[Int] ) : List[Int] = {
        val source = {
          def first  = (witnessProgram( startByte ) & 0xFF) << 8
          def second = if (startByte < len - 1) (witnessProgram( startByte + 1 ) & 0xFF) else 0
          first | second
        }
        val tailBits = 16 - (5 + startBit)
        val extracted = ((UnoffsetWindow5 >>> startBit) & source) >>> tailBits

        val nextReverseQuintetAccum = extracted :: reverseQuintetAccum
        val nextStartByte = if (startBit < 3) startByte else startByte + 1
        val nextStartBit  = (startBit + 5) % 8

        if ( nextStartByte >= len ) {
          nextReverseQuintetAccum.reverse
        }
        else {
          encode( nextStartByte, nextStartBit, nextReverseQuintetAccum )
        }
      }

      encode( 0, 0, Nil )
    }
    else {
      Nil
    }
  }

  private def packQuintets( quintets : Array[Int] ) : Array[Byte] = packQuintetsFrom( quintets, 0 )

  private def packQuintetsFrom( quintets : Array[Int], startIndex : Int ) : Array[Byte] = {
    val len = quintets.length

    if ( len > 0 ) {
      @tailrec
      def decode( startQuintet : Int, startBit : Int, reverseAccumBytes : List[Byte] ) : Array[Byte] = {
        // println( s"decode( ${startQuintet}, ${startBit}, ${reverseAccumBytes} )" )

        val source    = {
          def first5  = quintets(startQuintet    ) << 10
          def second5 = quintets(startQuintet + 1) <<  5
          def third5  = quintets(startQuintet + 2)

          (len - startQuintet) match {
            case 0 => sys.error( s"Oops. We have recursed into an index out of bounds situation, startQuintet ${startQuintet}, len ${len}" )
            case 1 => first5
            case 2 => first5 | second5
            case _ => first5 | second5 | third5
          }
        }

        // println( s"source: ${zlp15_binaryString_delimited(source)}" )

        val tailBits  = 15 - (8 + startBit)
        val extracted = (((UnoffsetWindow8 >>> startBit) & source) >>> tailBits)

        // println( s"startBit: ${startBit}, tailBits: ${tailBits}" )
        // println( s"extracted: ${zlp8_binaryString(extracted)}" )

        val nextReverseAccumBytes = extracted.toByte :: reverseAccumBytes

        val nextStartQuintet = if (startBit < 2) startQuintet + 1 else startQuintet + 2
        val nextStartBit  = (5 - (tailBits % 5)) % 5

        // println( s"nextStartQuintet: ${nextStartQuintet}, len: ${len}" )

        def nextOverflowsLastByte : Boolean = {
          val out = nextStartQuintet == len - 1 && nextStartBit != 0
          if ( out ) {
            //println( s"nextStartBit: ${nextStartBit}, quintet: ${zlp5_binaryString(quintets(len-1))}}" )
            val tailMask = (1 << (5 - nextStartBit)) - 1
            if ((quintets( len - 1 ) & tailMask) != 0) {
              throw new InvalidSegWitException( s"Any incomplete group at the end MUST be all zeroes, but is not -- nextStartBit: ${nextStartBit}, quintet: ${zlp5_binaryString(quintets(len-1))}}" )
            }
          }
          out
        }

        def checkTerminate : Boolean = {
          nextStartQuintet >= len || nextOverflowsLastByte
        }

        if ( checkTerminate ) {
          nextReverseAccumBytes.reverse.toArray
        }
        else {
          decode( nextStartQuintet, nextStartBit, nextReverseAccumBytes )
        }
      }

      decode( startIndex, 0, Nil )
    }
    else {
      Array.empty
    }
  }

  // debugging only
  private def zlpn_binaryString( n : Int, i : Int ) = {
    val tail = i.toBinaryString
    if ( tail.length < n ) {
      ("0"*(n-tail.length)) + tail
    }
    else if (tail.length == n ){
      tail
    }
    else {
      tail.drop( tail.length - n )
    }
  }

  private def zlp5_binaryString( i : Int ) = zlpn_binaryString( 5, i )
  private def zlp8_binaryString( i : Int ) = zlpn_binaryString( 8, i )
  private def zlp15_binaryString( i : Int ) = zlpn_binaryString( 15, i )

  private def zlp15_binaryString_delimited( i : Int ) = {
    val raw = zlp15_binaryString( i )
    raw.substring(0,5) + '|' + raw.substring(5,10) + '|' + raw.substring(10,15)
  }
}
