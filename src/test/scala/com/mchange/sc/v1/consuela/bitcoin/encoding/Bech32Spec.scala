package com.mchange.sc.v1.consuela.bitcoin.encoding

import scala.collection._

import org.specs2._

import com.mchange.sc.v1.consuela._

import com.mchange.sc.v3.failable._

object Bech32Spec {
  val ValidSuite = immutable.Seq( // from https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
    "A12UEL5L",
    "a12uel5l",
    "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs",
    "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw",
    "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j",
    "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w",
    "?1ezyfcl"
  )
  val InvalidSuite = immutable.Seq( // from https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
    0x20.toChar + "1nwldj5",
    0x7F.toChar + "1axkwrx",
    0x80.toChar + "1eym55h",
    "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx",
    "pzry9x0s0muk",
    "1pzry9x0s0muk",
    "x1b4n0q5v",
    "li1dgmt3",
    "de1lg7wt",
    "A1G7SGD8",
    "10a06t8",
    "1qzzfhee"
  )
}
class Bech32Spec extends Specification { def is = s2"""
   Suite of valid Bech32 parses                                 ${e1} 
   Suite of invalid Bech32 fails to parse                       ${e2} 
"""

  import Bech32Spec._

  def e1 : Boolean = ValidSuite.map( encoded => Bech32.decodeQuintets( None, encoded ) ).forall( _ != null )
  def e2 : Boolean = InvalidSuite.map( encoded => Failable( Bech32.decodeQuintets( None, encoded ) ) ).forall( _.isFailed )
}

