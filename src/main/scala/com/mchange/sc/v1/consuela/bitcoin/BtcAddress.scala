package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin.encoding.Base58

import scala.collection._

object BtcAddress {

  private final val Byte0 = 0.toByte

  private final val Byte_UnversionedPubKeyHashLength = 0x14.toByte

  // see https://en.bitcoin.it/wiki/Script
  private final object OP {
    val CHECKSIG    : Byte = 0xac.toByte
    val DUP         : Byte = 0x76.toByte
    val EQUALVERIFY : Byte = 0x88.toByte 
    val HASH160     : Byte = 0xa9.toByte
  }

  // see https://eips.ethereum.org/EIPS/eip-2304
  //     https://en.bitcoin.it/wiki/Transaction#Types_of_Transaction
  private object P2PKH {
    val ScriptPubKeyLength = 25
    private val Template = {
      val raw = Array.ofDim[Byte](ScriptPubKeyLength)
      raw(0)  = OP.DUP
      raw(1)  = OP.HASH160
      raw(2)  = Byte_UnversionedPubKeyHashLength
      raw(23) = OP.EQUALVERIFY
      raw(24) = OP.CHECKSIG
      raw
    }
    def scriptPubKeyFor( publicKeyHash : ByteSeqExact20 ) : immutable.Seq[Byte] = {
      val in = publicKeyHash.widen.toArray
      val out = Template.clone()
      Array.copy( in, 0, out, 3, 20 )
      out.toImmutableSeq
    }
  }
  case object P2PKH_Mainnet extends Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2PKH_Mainnet = this.apply( Base58.encodeChecked( 0x00, publicKeyHash.widen.toArray ) )
  }
  // case object P2SH_0x05 extends Type
  // case object SegWit_bc extends Type
  sealed trait Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : BtcAddress
  }

  abstract class P2PKH( requiredVersion : Byte, text : String ) extends BtcAddress {
    val ( version, toPublicKeyHash ) = {
      val ( v, arr ) = Base58.decodeChecked(text)
      require( v == requiredVersion, s"${this} must encode a ${requiredVersion} header byte, '${text}' encodes version ${v}." )
      (v, ByteSeqExact20(arr) )
    }
    lazy val toScriptPubKey = P2PKH.scriptPubKeyFor( toPublicKeyHash )
  }

  final case class P2PKH_Mainnet( val text : String ) extends P2PKH( Byte0, text )
}
sealed trait BtcAddress {
  def text : String
  def toPublicKeyHash : ByteSeqExact20
  def toScriptPubKey : immutable.Seq[Byte]
}
