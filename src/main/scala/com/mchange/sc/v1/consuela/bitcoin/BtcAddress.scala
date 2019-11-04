package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin.encoding.Base58

import com.mchange.sc.v3.failable._;

import scala.collection._

object BtcAddress {

  private final val Byte_UnversionedPubKeyHashLength = 0x14.toByte

  // see https://en.bitcoin.it/wiki/Script
  private final object OP {
    val CHECKSIG    : Byte = 0xac.toByte
    val DUP         : Byte = 0x76.toByte
    val EQUAL       : Byte = 0x87.toByte
    val EQUALVERIFY : Byte = 0x88.toByte
    val HASH160     : Byte = 0xa9.toByte
  }

  sealed trait Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : BtcAddress
  }

  private def publicKeyHashFromBase58Checked( requiredVersion : Byte )( version : Byte, payload : Array[Byte] ) : ByteSeqExact20 = {
    require( version == requiredVersion, s"${this} must include a ${requiredVersion} header byte, instead header byte is ${version}." )
    ByteSeqExact20( payload )
  }

  // see https://eips.ethereum.org/EIPS/eip-2304
  //     https://en.bitcoin.it/wiki/Transaction#Types_of_Transaction
  private final object P2PKH {
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

    final object Version {
      final val Mainnet : Byte = 0x00.toByte
    }
  }

  final case object P2PKH_Mainnet extends Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2PKH_Mainnet = {
      val version = P2PKH.Version.Mainnet
      val payload = publicKeyHash.widen.toArray
      val text = Base58.encodeChecked( version, payload )
      this.apply( text )
    }
  }
  final case class P2PKH_Mainnet( val text : String ) extends BtcAddress {
    private val ( version, payload ) = Base58.decodeChecked( text ) // not lazy, insist on all checks
    val toPublicKeyHash = publicKeyHashFromBase58Checked( P2PKH.Version.Mainnet )( version, payload )
    val toScriptPubKey  = P2PKH.scriptPubKeyFor( toPublicKeyHash )
  }

  // see https://eips.ethereum.org/EIPS/eip-2304
  //     https://en.bitcoin.it/wiki/Transaction#Types_of_Transaction
  private final object P2SH {
    val ScriptPubKeyLength = 23
    private val Template = {
      val raw = Array.ofDim[Byte](ScriptPubKeyLength)
      raw(0)  = OP.HASH160
      raw(1)  = Byte_UnversionedPubKeyHashLength
      raw(22) = OP.EQUAL
      raw
    }
    def scriptPubKeyFor( publicKeyHash : ByteSeqExact20 ) : immutable.Seq[Byte] = {
      val in = publicKeyHash.widen.toArray
      val out = Template.clone()
      Array.copy( in, 0, out, 2, 20 )
      out.toImmutableSeq
    }

    final object Version {
      final val Mainnet : Byte = 0x05.toByte
    }
  }

  final case object P2SH_Mainnet extends Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2SH_Mainnet = {
      val version = P2SH.Version.Mainnet
      val payload = publicKeyHash.widen.toArray
      val text = Base58.encodeChecked( version, payload )
      this.apply( text )
    }
  }
  final case class P2SH_Mainnet( val text : String ) extends BtcAddress {
    private val ( version, payload ) = Base58.decodeChecked( text ) // not lazy, insist on all checks
    val toPublicKeyHash = publicKeyHashFromBase58Checked( P2SH.Version.Mainnet )( version, payload )
    val toScriptPubKey = P2SH.scriptPubKeyFor( toPublicKeyHash )
  }

  // case object SegWit_bc extends Type

  def parse( text : String ) : Failable[BtcAddress] = {
    Failable {
      val ( version, payload ) = Base58.decodeChecked( text )
      version match {
        case P2PKH.Version.Mainnet => P2PKH_Mainnet( text ) // we'll decodeChecked Base58 redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case P2SH.Version.Mainnet  => P2SH_Mainnet ( text ) // we'll decodeChecked Base58 redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case _                     => throw new UnknownBtcAddressFormatException( s"Couldn't parse '${text}, decodes as Base58check, but unexpected version ${version}'" )
      }
    }
  }
}
sealed trait BtcAddress {
  def text            : String
  def toPublicKeyHash : ByteSeqExact20
  def toScriptPubKey  : immutable.Seq[Byte]
}
