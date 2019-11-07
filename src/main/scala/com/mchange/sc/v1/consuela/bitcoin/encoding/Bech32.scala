package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela._

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

  private final val Padding6 = Array.ofDim[Int](6)

  def decodeQuintets( expectedHrp : Option[String], encoded : String ) : Iterable[Int] = {
    if ( encoded.isMixedCase ) {
      throw new InvalidBech32Exception( s"Mixed case encodings are forbidden: '${encoded}'" )
    }
    val caseNormalized = normalizeCase( encoded )
    val (hrp, dataWithChecksum) = splitValidateHrpData( caseNormalized )

    expectedHrp.foreach { expected =>
      if ( expected.isMixedCase ) {
        throw new IllegalArgumentException( s"Expected Human Readable Part should not be mixed-case. expectedHrp: ${expected}" )
      }
      else if ( normalizeCase( expected ) != hrp ) {
        throw new InvalidBech32Exception( s"Did not find expected human-readable part (hrp). found: '${hrp}', expected: ${expectedHrp}" )
      }
    }

    if (! verifyChecksum( hrp, dataWithChecksum ) ) {
      throw new Bech32ChecksumFailedException( s"Bad address. Checksum failed: ${encoded}" )
    }

    val unchecksummed = unchecksum( dataWithChecksum )

    unchecksummed.map( c => AlphabetToIndex( c ) )
  }

  def encodeQuintets( hrp : String, quintets : Array[Int] ) : String = {

    whyBadHrp( hrp ).foreach( msg => throw new IllegalArgumentException( msg ) )

    val nhrp = normalizeCase(hrp)
    val checksum = createChecksum( nhrp, quintets )
    val out = ( nhrp + '1' + toStringQuintets( quintets ) + checksum ).toLowerCase
    if ( out.length > 90 ) {
      throw new InvalidBech32Exception( s"Cannot encode, would illegally yield Bech32 longer than 90 chars (${out.length} chars: '${out}'), which is illegal." )
    }
    out
  }

  private def unchecksum( dataWithChecksum : String ) : String = dataWithChecksum.substring( 0, dataWithChecksum.length - 6 )

  private def normalizeCase( encoded : String ) : String = {
    encoded.toLowerCase
  }

  private def whyBadHrp( hrp : String ) : Option[String] = {
    val len = hrp.length

    if (len < 1) {
      Some( s"Human readable part must be at least one character long, is not" )
    }
    else if (len > 83) {
      Some( s"Human readable part can be no longer than 83 chars, found ${len}" )
    }
    else if ( hrp.exists( c => (c < 33 || c > 126) ) ) {
      Some( s"Human readable part must include only ASCII characters in range [33,126] (hrp: '${hrp}')" )
    }
    else if ( hrp.isMixedCase ) {
      Some( s"Mixed case human-readable parts are forbidden. (hrp: '${hrp}')" )
    }
    else {
      None
    }
  }

  private def splitValidateHrpData( encoded : String ) : (String, String) = {
    val separator = encoded.lastIndexOf('1')

    if (separator < 0) {
      throw new InvalidBech32Exception( s"No delimited human readable part: '${encoded}'" )
    }

    // note that we are skipping the separator char
    val hrp  = encoded.substring(0, separator)
    val data = encoded.substring(separator + 1)

    whyBadHrp( hrp ).foreach( msg => throw new InvalidBech32Exception( s"${msg}: '${encoded}'" ) )

    if ( data.length < 6 ) {
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

  private def createChecksum( hrp : String, quintets : Array[Int] ) : String = {
    val paddedValues = Array.concat( hrpExpand(hrp), quintets, Padding6 )
    val pmod         = polymod( paddedValues ) ^ 1
    (0 until 6 ).map( i => (pmod >>> (5 * (5-i))) & 31 ).map( IndexToAlphabet ).mkString
  }
}
