package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin.encoding.{Base58,SegWit}

import com.mchange.sc.v3.failable._

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

  private def toHex( b : Byte ) = String.format("%02X ", Array.ofDim[Byte](b))

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

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if (scriptPubKey.length != P2PKH.ScriptPubKeyLength) {
        Some( s"P2PKH addresses should yield scriptPubKeys of length ${P2PKH.ScriptPubKeyLength}, cannot parse from ${scriptPubKey} with length ${scriptPubKey.length}" )
      }
      else if (scriptPubKey(0) != OP.DUP) {
        Some( s"P2PKH scriptPubkey should, doesn't, begin with OP_DUP (0x${toHex(OP.DUP)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(1) != OP.HASH160) {
        Some( s"The second byte of a P2PKH scriptPubkey should be, isn't, OP_HASH160 (0x${toHex(OP.HASH160)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(2) != Byte_UnversionedPubKeyHashLength ) {
        Some( s"The third byte of a P2PKH scriptPubKey should be the public key hash length 0x${toHex(Byte_UnversionedPubKeyHashLength)}, isn't. scriptPubKey: 0x${scriptPubKey.hex}"  )
      }
      else if ( scriptPubKey(23) != OP.EQUALVERIFY ) {
        Some( s"The next to last byte of a P2PKH scriptPubkey should be, isn't, OP_EQUALVERIFY (0x${toHex(OP.EQUALVERIFY)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(24) != OP.CHECKSIG ) {
        Some( s"P2PKH scriptPubkey should, doesn't, end with OP_CHECKSIG (0x${toHex(OP.CHECKSIG)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else {
        None
      }
    }

    private[BtcAddress]
    def extractPublicKeyHash( scriptPubKey : immutable.Seq[Byte] ) : ByteSeqExact20 = ByteSeqExact20( scriptPubKey.slice( 3, 23 ) )

    final object Version {
      final val Mainnet : Byte = 0x00.toByte
    }
  }

  final case object P2PKH_Mainnet extends Type {
    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2PKH_Mainnet = {
      P2PKH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromPublicKeyHash( P2PKH.extractPublicKeyHash( scriptPubKey ) )
      }
    }
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2PKH_Mainnet = {
      val version = P2PKH.Version.Mainnet
      val payload = publicKeyHash.widen.toArray
      val text = Base58.encodeChecked( version, payload )
      this.apply( text )
    }
  }
  final case class P2PKH_Mainnet( val text : String ) extends HashRetrievable {
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

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if (scriptPubKey.length != P2SH.ScriptPubKeyLength) {
        Some( s"P2SH addresses should yield scriptPubKeys of length ${P2SH.ScriptPubKeyLength}, cannot parse from ${scriptPubKey} with length ${scriptPubKey.length}" )
      }
      else if (scriptPubKey(0) != OP.HASH160) {
        Some( s"P2SH scriptPubkey should, doesn't, begin with OP_HASH160 (0x${toHex(OP.HASH160)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(1) != Byte_UnversionedPubKeyHashLength )
        Some( s"The second byte of a P2SH scriptPubKey should be the public key hash length 0x${toHex(Byte_UnversionedPubKeyHashLength)}, isn't. scriptPubKey: 0x${scriptPubKey.hex}"  )
      else if ( scriptPubKey(22) != OP.EQUAL ) {
        Some( s"P2SH scriptPubkey should, doesn't, end with OP_EQUAL (0x${toHex(OP.EQUAL)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else {
        None
      }
    }

    private[BtcAddress]
    def extractPublicKeyHash( scriptPubKey : immutable.Seq[Byte] ) : ByteSeqExact20 = ByteSeqExact20( scriptPubKey.slice( 2, 22 ) )

    final object Version {
      val Mainnet = 0x05.toByte
    }
  }

  final case object P2SH_Mainnet extends Type {

    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2SH_Mainnet = {
      P2SH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromPublicKeyHash( P2SH.extractPublicKeyHash( scriptPubKey ) )
      }
    }
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2SH_Mainnet = {
      val version = P2SH.Version.Mainnet
      val payload = publicKeyHash.widen.toArray
      val text = Base58.encodeChecked( version, payload )
      this.apply( text )
    }
  }
  final case class P2SH_Mainnet( val text : String ) extends HashRetrievable {
    private val ( version, payload ) = Base58.decodeChecked( text ) // not lazy, insist on all checks
    require( version == P2SH.Version.Mainnet, s"Bad P2SH_Mainnet Version: ${version}" )
    val toPublicKeyHash = publicKeyHashFromBase58Checked( P2SH.Version.Mainnet )( version, payload )
    val toScriptPubKey = P2SH.scriptPubKeyFor( toPublicKeyHash )
  }

  private final object SegWitHumanReadablePart {
    val Mainnet = "bc"
    val Testnet = "tb"
  }

  private final object P2WPKH {
    val Version = 0x00.toByte

    val WitnessProgramLen = 20

    val ScriptPubKeyLen = 22

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if ( scriptPubKey.length != ScriptPubKeyLen ) {
        Some( s"A P2WKH public key must be ${ScriptPubKeyLen} bytes long, found ${scriptPubKey.length} bytes: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(0) != Version ) {
        Some( s"The first (version) byte of a P2WPKH hash should be ${toHex(Version)}, found ${toHex(scriptPubKey(0))}: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(1) != Byte_UnversionedPubKeyHashLength ) {
        Some( s"The second byte of a P2WPKH hash should be the public key hash length ${toHex(Byte_UnversionedPubKeyHashLength)}, found ${toHex(scriptPubKey(0))}: 0x${scriptPubKey.hex}" )
      }
      else {
        None
      }
    }

    private[BtcAddress]
    def extractPublicKeyHash( scriptPubKey : immutable.Seq[Byte] ) : ByteSeqExact20 = ByteSeqExact20( scriptPubKey.drop(2) )

    val HumanReadablePart = SegWitHumanReadablePart
  }
  final case object P2WPKH_Mainnet extends Type {
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2WPKH_Mainnet = this.apply( SegWit.encode(P2WPKH.HumanReadablePart.Mainnet,P2WPKH.Version,publicKeyHash.widen) )

    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2WPKH_Mainnet = {
      P2WPKH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromPublicKeyHash( P2WPKH.extractPublicKeyHash( scriptPubKey ) )
      }
    }
  }
  final case class P2WPKH_Mainnet( val text : String ) extends HashRetrievable {
    val witnessProgram = {
      val ( version, wp ) = SegWit.decode(Some(P2WPKH.HumanReadablePart.Mainnet), text)
      require(
        version == P2WPKH.Version && wp.length == P2WPKH.WitnessProgramLen,
        s"Bad address '${text}': P2WPKH_Mainnet should specify version ${P2WPKH.Version} and a ${P2WPKH.WitnessProgramLen}-byte witness program. Found version ${version} and a ${wp.length} byte witness program."
      )
      wp
    }

    val toPublicKeyHash = ByteSeqExact20( witnessProgram )
    val toScriptPubKey = {
      val buffer = new mutable.ArrayBuffer[Byte](P2WPKH.ScriptPubKeyLen)
      buffer += P2WPKH.Version
      buffer += Byte_UnversionedPubKeyHashLength
      buffer ++= witnessProgram
      buffer.toArray.toImmutableSeq
    }
  }

  sealed trait HashRetrievable extends BtcAddress {
    def toPublicKeyHash : ByteSeqExact20
  }

  // case object SegWit_bc extends Type

  def parse( text : String ) : Failable[BtcAddress] = {
    def mainnetSegWitAttempts = Failable {
      val ( version, witnessProgram ) = SegWit.decode( Some(SegWitHumanReadablePart.Mainnet), text )
      ( version, witnessProgram.length ) match {
        case ( 0, P2WPKH.WitnessProgramLen ) => P2WPKH_Mainnet( text ) // we'll decode SegWit redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case other => throw new UnknownBtcAddressFormatException( s"Couldn't parse '${text}, decodes as SegWit, but unexpected version ${version} and or witness program length ${witnessProgram.length}: ${other} " )
      }
    }
    def base58Attempts = Failable {
      val ( version, payload ) = Base58.decodeChecked( text )
      version match {
        case P2PKH.Version.Mainnet => P2PKH_Mainnet( text ) // we'll decodeChecked Base58 redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case P2SH.Version.Mainnet  => P2SH_Mainnet ( text ) // we'll decodeChecked Base58 redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case _                     => throw new UnknownBtcAddressFormatException( s"Couldn't parse '${text}, decodes as Base58check, but unexpected version ${version}'" )
      }
    }

    mainnetSegWitAttempts orElseTrace base58Attempts
  }

  def recoverFromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Failable[BtcAddress] = {
    Failable( P2WPKH_Mainnet.fromScriptPubKey( scriptPubKey) ) orElseTrace
    Failable( P2PKH_Mainnet.fromScriptPubKey( scriptPubKey ) ) orElseTrace
    Failable( P2SH_Mainnet.fromScriptPubKey( scriptPubKey ) )
  }

  // throws exceptions on failure
  def apply( text : String ) : BtcAddress = parse( text ).assert
}
sealed trait BtcAddress {
  def text            : String
  def toScriptPubKey  : immutable.Seq[Byte]
}
