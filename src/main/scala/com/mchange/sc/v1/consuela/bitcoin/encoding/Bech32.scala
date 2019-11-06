package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._

import scala.annotation.tailrec
import scala.collection._

// see https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
//     https://eips.ethereum.org/EIPS/eip-2304
//     https://medium.com/@MeshCollider/some-of-the-math-behind-bech32-addresses-cf03c7496285

object Bech32 {
  private final val Alphabet        = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
  private final val AlphabetSet     = Alphabet.toSet
  private final val AlphabetLength  = Alphabet.length
  private final val AlphabetToIndex = Alphabet.zipWithIndex.toMap
  private final val IndexToAlphabet = AlphabetToIndex.map{ case ( k, v ) => ( v, k ) }.toMap
  
  private final val GEN = Array[Int]( 0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3 )

  private final val Mask5           = (1 << 5) - 1
  private final val UpperMask5      = (Mask5 << 5)
  private final val UnoffsetWindow5 = (Mask5 << 11)
  private final val UnoffsetWindow8 = (0xFF  << 7)

  private final val Padding6 = Array.ofDim[Int](6)

  def decodeSegWit( expectedHrp : String, encoded : String ) : ( Byte, immutable.Seq[Byte] ) = {
    val ( v, w ) = decodeSegWitAsArray( expectedHrp : String, encoded : String )
    ( v, w.toImmutableSeq )
  }

  def decodeSegWitAsArray( expectedHrp : String, encoded : String ) : ( Byte, Array[Byte] ) = {
    val caseNormalized = normalizeCase( encoded )
    val (hrp, stringQuintetsWithChecksum) = splitValidateHrpData( caseNormalized )

    if ( normalizeCase( expectedHrp ) != hrp ) {
      throw new InvalidBech32Exception( s"Did not find expected human-readable part (hrp). found: '${hrp}', expected: ${expectedHrp}" )
    }
    else if (! verifyChecksum( hrp, stringQuintetsWithChecksum ) ) {
      throw new InvalidBech32Exception( s"Bad address. Checksum failed: ${encoded}" )
    }

    val unchecksummedStringQuintets = unchecksum( stringQuintetsWithChecksum )

    val out = decodeUnchecksummed( unchecksummedStringQuintets )

    ensureValidVersionWitnessProgramPairTupled(out)

    out
  }

  def encodeSegWit( hrp : String, version : Byte, witnessProgram : Array[Byte] ) : String = {
    ensureValidVersionWitnessProgramPair( version, witnessProgram )
    val checksum = createChecksum( hrp, version, witnessProgram )
    hrp + '1' + toCharQuintet(version) + expandToStringQuintets( witnessProgram ) + checksum
  }

  private def ensureValidVersionWitnessProgramPair( version : Byte, witnessProgram : Array[Byte] ) : Unit = {
    ( version, witnessProgram.length ) match {
      case ( 0, 20 )   => ()
      case ( 0, 32 )   => ()
      case ( 0, len  ) => throw new InvalidBech32Exception( s"Version 0x0 addresses must have 20 or 32 byte witness programs, found ${len} bytes" )
      case _           => ()
    }
  }

  private val ensureValidVersionWitnessProgramPairTupled = (ensureValidVersionWitnessProgramPair _).tupled

  private def unchecksum( dataWithChecksum : String ) : String = dataWithChecksum.substring( 0, dataWithChecksum.length - 6 )

  private def decodeUnchecksummed( stringQuintets : String ) : ( Byte, Array[Byte] ) = ( toQuintet(stringQuintets.head).toByte, packQuintets( stringQuintets.tail ) )

  private def zlpn_binaryString( n : Int, i : Int ) = {
    val tail = i.toBinaryString
    if ( tail.length < n ) {
      ("0"*(n-tail.length)) + tail
    }
    else {
      tail
    }
  }

  private def expandToQuintets( witnessPart : Array[Byte] ) : Iterable[Int] = {

    val len = witnessPart.length

    def encode( startByte : Int, startBit : Int, reverseQuintetAccum : List[Int] ) : List[Int] = {
      val source = {
        def first  = (witnessPart( startByte ) & 0xFF) << 8
        def second = if (startByte < len - 1) (witnessPart( startByte + 1 ) & 0xFF) else 0
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

  private def expandToStringQuintets( witnessPart : Array[Byte] ) : String = {
    toStringQuintets( expandToQuintets( witnessPart ) )
  }

  private def packQuintets( stringQuintets : String ) : Array[Byte] = packQuintetsFrom( stringQuintets, 0 )

  private def packQuintetsFrom( stringQuintets : String, startIndex : Int ) : Array[Byte] = {
    val len = stringQuintets.length

    @tailrec
    def decode( startChar : Int, startBit : Int, reverseAccumBytes : List[Byte] ) : Array[Byte] = {
      // println( s"decode( ${startChar}, ${startBit}, ${reverseAccumBytes} )" )

      val source    = {
        def first5  = (AlphabetToIndex(stringQuintets.charAt(startChar    )) & Mask5) << 10
        def second5 = (AlphabetToIndex(stringQuintets.charAt(startChar + 1)) & Mask5) <<  5
        def third5  = (AlphabetToIndex(stringQuintets.charAt(startChar + 2)) & Mask5)

        (len - startChar) match {
          case 0 => sys.error( s"Oops. We have recursed into an index out of bounds situation, startChar ${startChar}, len ${len}" )
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

      val nextStartChar = if (startBit < 2) startChar + 1 else startChar + 2
      val nextStartBit  = (5 - (tailBits % 5)) % 5

      // println( s"nextStartChar: ${nextStartChar}, len: ${len}" )

      if ( nextStartChar >= len ) {
        nextReverseAccumBytes.reverse.toArray
      }
      else {
        decode( nextStartChar, nextStartBit, nextReverseAccumBytes )
      }
    }

    decode( startIndex, 0, Nil )
  }

  private def normalizeCase( encoded : String ) : String = {
    if ( encoded.isMixedCase ) {
      throw new InvalidBech32Exception( s"Mixed case encodings are forbidden: '${encoded}'" )
    }
    else {
      encoded.toLowerCase
    }
  }

  private def splitValidateHrpData( encoded : String ) : (String, String) = {
    val separator = encoded.lastIndexOf('1')

    if (separator < 0) {
      throw new InvalidBech32Exception( s"No delimited human readable part: '${encoded}'" )
    }
    else if (separator < 1) {
      throw new InvalidBech32Exception( s"Human readable part must be at least one character long: '${encoded}'" )
    }

    val hrp  = encoded.substring(0, separator)
    val data = encoded.substring(separator + 1)

    if ( hrp.exists( c => (c < 33 || c > 126) ) ) {
      throw new InvalidBech32Exception( s"Human readable part must include only ASCII characters in range [33,126]. hrp: '${hrp}'" )
    }
    else if ( data.length < 6 ) {
      throw new InvalidBech32Exception( s"Data part must be at least 6 characters long. data: '${data}'" )
    }
    else if ( data.exists( c => !AlphabetSet(c) ) ) {
      throw new InvalidBech32Exception( s"Only characters in ${AlphabetSet} permitted in data part. data: '${data}'" )
    }

    ( hrp, data )
  }

  private def hrpExpand( hrp : String ) : Array[Int] = {
    val hrparr = hrp.toArray
    val hrplen = hrp.length
    val prefix = hrparr.map( _ >>> 5 )
    val suffix = hrparr.map( _ & 31 )

    val out = Array.ofDim[Int]( (2 * hrplen) + 1 ) // note the plus 1 for the zero gap
    Array.copy( prefix, 0, out, 0, hrplen )
    Array.copy( suffix, 0, out, hrplen + 1, hrplen ) // note that we are leaving a one-zero gap, this is not a concatenation
    out
  }

  private def polymod( values : Array[Int] ) : Int = {
    var chk = 1
    values foreach { v =>
      val b = chk >>> 25
      chk = ((chk & 0x1ffffff) << 5) ^ v
      (0 until 5 ) foreach { i =>
        chk = if (((b >>> i) & 1) != 0) (chk ^ GEN(i)) else chk
      }
    }
    chk
  }

  private def toQuintets( data : String ) : Array[Int] = data.map( AlphabetToIndex ).toArray

  private def toStringQuintets( quintets : Iterable[Int] ) : String = quintets.map( IndexToAlphabet ).mkString

  private def toQuintet( c : Char ) : Int = AlphabetToIndex(c)

  private def toCharQuintet( i : Int ) : Char = IndexToAlphabet(i)

  private def verifyChecksum( hrp : String, stringQuintets : String ) : Boolean = {
    // println( s"verifyChecksum( ${hrp}, ${data} )" )
    polymod( Array.concat( hrpExpand(hrp), toQuintets(stringQuintets) ) ) == 1
  }

  private def createChecksum( hrp : String, version : Byte, witnessProgram : Array[Byte] ) : String = {
    val data = Array.concat( Array[Int]( version ), toQuintets( expandToStringQuintets(witnessProgram) ) )
    createChecksum( hrp, data )
  }

  private def createChecksum( hrp : String, quintets : Array[Int] ) : String = {
    val paddedValues = Array.concat( hrpExpand(hrp), quintets, Padding6 )
    val pmod         = polymod( paddedValues ) ^ 1
    (0 until 6 ).map( i => (pmod >>> (5 * (5-i))) & 31 ).map( IndexToAlphabet ).mkString
  }

  // debugging only
  private def zlp8_binaryString( i : Int ) = zlpn_binaryString( 8, i )
  private def zlp15_binaryString( i : Int ) = zlpn_binaryString( 15, i )

  private def zlp15_binaryString_delimited( i : Int ) = {
    val raw = zlp15_binaryString( i )
    raw.substring(0,5) + '|' + raw.substring(5,10) + '|' + raw.substring(10,15)
  }
}
