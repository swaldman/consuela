package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.bitcoin.{encoding => enc}

import com.mchange.sc.v3.failable._

import scala.collection._

object BtcAddress {

  private final val Byte_Hash160Length = 0x14.toByte
  private final val Byte32             = 0x20.toByte

  // see https://en.bitcoin.it/wiki/Script
  private final object OP {
    val CHECKSIG    : Byte = 0xac.toByte
    val DUP         : Byte = 0x76.toByte
    val EQUAL       : Byte = 0x87.toByte
    val EQUALVERIFY : Byte = 0x88.toByte
    val HASH160     : Byte = 0xa9.toByte

    def apply( i : Int ) : Byte = {
      i match {
        case 0                     => 0x00.toByte
        case i if i > 0 && i <= 16 => (0x50 + i).toByte
        case other                 => throw new IllegalArgumentException( s"No OP_${i} has been defined." )
      }
    }
  }

  case class Base58( version : Byte ) extends Encoding
  case class SegWit( version : Byte, payloadLength : Int, hrp : String ) extends Encoding
  sealed trait Encoding

  sealed trait Type {
    def encoding : Encoding
    def fromPayload( payload : immutable.Seq[Byte] ) : BtcAddress
    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : BtcAddress
  }

  private def toHex( b : Byte ) = String.format("%02X ", Array.ofDim[Byte](b))

  // see https://eips.ethereum.org/EIPS/eip-2304
  //     https://en.bitcoin.it/wiki/Transaction#Types_of_Transaction



  /*
   *  Base58 formats
   */ 

  /* P2PKH */

  private final object P2PKH {
    val ScriptPubKeyLen = 25
    val PublicKeyHashLen   = 20

    private val Template = {
      val raw = Array.ofDim[Byte](ScriptPubKeyLen)
      raw(0)  = OP.DUP
      raw(1)  = OP.HASH160
      raw(2)  = Byte_Hash160Length
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
      if (scriptPubKey.length != P2PKH.ScriptPubKeyLen) {
        Some( s"P2PKH addresses should yield scriptPubKeys of length ${P2PKH.ScriptPubKeyLen}, cannot parse from ${scriptPubKey} with length ${scriptPubKey.length}" )
      }
      else if (scriptPubKey(0) != OP.DUP) {
        Some( s"P2PKH scriptPubkey should, doesn't, begin with OP_DUP (0x${toHex(OP.DUP)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(1) != OP.HASH160) {
        Some( s"The second byte of a P2PKH scriptPubkey should be, isn't, OP_HASH160 (0x${toHex(OP.HASH160)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(2) != Byte_Hash160Length ) {
        Some( s"The third byte of a P2PKH scriptPubKey should be the public key hash length 0x${toHex(Byte_Hash160Length)}, isn't. scriptPubKey: 0x${scriptPubKey.hex}"  )
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
    val encoding = Base58( P2PKH.Version.Mainnet )
    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2PKH_Mainnet = {
      P2PKH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromPublicKeyHash( P2PKH.extractPublicKeyHash( scriptPubKey ) )
      }
    }
    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2PKH_Mainnet = {
      val version = P2PKH.Version.Mainnet
      val payload = publicKeyHash.widen.toArray
      val text = enc.Base58.encodeChecked( version, payload )
      this.apply( text )
    }
    def fromPayload( payload : immutable.Seq[Byte] ) : P2PKH_Mainnet = this.fromPublicKeyHash( ByteSeqExact20( payload ) )
  }
  final case class P2PKH_Mainnet( val text : String ) extends PublicKeyHashRecoverable {
    private val ( version, _payload ) = enc.Base58.decodeChecked( text ) // not lazy, insist on all checks
    require( version == P2PKH.Version.Mainnet, s"Bad P2PKH_Mainnet Version: ${version}" )
    require( _payload.length == P2PKH.PublicKeyHashLen, s"Bad P2PKH_Mainnet public key hash. Must be ${P2PKH.PublicKeyHashLen} bytes, found ${_payload.length} bytes. publicKeyHash: 0x${_payload}" )
    val payload = _payload.toImmutableSeq
    val toPublicKeyHash = ByteSeqExact20( payload )
    val toScriptPubKey  = P2PKH.scriptPubKeyFor( toPublicKeyHash )
    def addressType = P2PKH_Mainnet
  }

  /* P2SH */

  private final object P2SH {
    val ScriptPubKeyLen = 23
    val ScriptHashLen   = 20

    private val Template = {
      val raw = Array.ofDim[Byte](ScriptPubKeyLen)
      raw(0)  = OP.HASH160
      raw(1)  = Byte_Hash160Length
      raw(22) = OP.EQUAL
      raw
    }
    def scriptPubKeyFor( scriptHash : Array[Byte] ) : immutable.Seq[Byte] = {
      require( scriptHash.length == ScriptHashLen, s"The payload of a P2SH address should be ${ScriptHashLen} bytes long, found ${scriptHash.length}. scriptHash: 0x${scriptHash.hex}" )
      val in = scriptHash
      val out = Template.clone()
      Array.copy( in, 0, out, 2, ScriptHashLen )
      out.toImmutableSeq
    }

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if (scriptPubKey.length != P2SH.ScriptPubKeyLen) {
        Some( s"P2SH addresses should yield scriptPubKeys of length ${P2SH.ScriptPubKeyLen}, cannot parse from ${scriptPubKey} with length ${scriptPubKey.length}" )
      }
      else if (scriptPubKey(0) != OP.HASH160) {
        Some( s"P2SH scriptPubkey should, doesn't, begin with OP_HASH160 (0x${toHex(OP.HASH160)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else if (scriptPubKey(1) != Byte_Hash160Length )
        Some( s"The second byte of a P2SH scriptPubKey should be the script hash length 0x${toHex(Byte_Hash160Length)}, isn't. scriptPubKey: 0x${scriptPubKey.hex}"  )
      else if ( scriptPubKey(22) != OP.EQUAL ) {
        Some( s"P2SH scriptPubkey should, doesn't, end with OP_EQUAL (0x${toHex(OP.EQUAL)})). scriptPubKey: 0x${scriptPubKey.hex}" )
      }
      else {
        None
      }
    }

    private[BtcAddress]
    def extractScriptHash( scriptPubKey : immutable.Seq[Byte] ) : ByteSeqExact20 = ByteSeqExact20( scriptPubKey.slice( 2, 22 ) )

    final object Version {
      val Mainnet = 0x05.toByte
    }
  }

  final case object P2SH_Mainnet extends Type {
    val encoding = Base58( P2SH.Version.Mainnet )

    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2SH_Mainnet = {
      P2SH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromScriptHash( P2SH.extractScriptHash( scriptPubKey ) )
      }
    }
    def fromScriptHash( scriptHash : ByteSeqExact20 ) : P2SH_Mainnet = {
      val version = P2SH.Version.Mainnet
      val payload = scriptHash.widen.toArray
      val text = enc.Base58.encodeChecked( version, payload )
      this.apply( text )
    }
    def fromPayload( bytes : immutable.Seq[Byte] )          : P2SH_Mainnet = this.fromScriptHash( ByteSeqExact20( bytes ) )
  }
  final case class P2SH_Mainnet( val text : String ) extends BtcAddress {
    private val ( version, _scriptHash ) = enc.Base58.decodeChecked( text ) // not lazy, insist on all checks
    require( version == P2SH.Version.Mainnet, s"Bad P2SH_Mainnet Version: ${version}" )
    require( _scriptHash.length == P2SH.ScriptHashLen, s"Bad P2SH_Mainnet scriptHash. Must be ${P2SH.ScriptHashLen} bytes, found ${_scriptHash.length} bytes. scriptHash: 0x${_scriptHash}" )
    val payload    = _scriptHash.toImmutableSeq
    val scriptHash = ByteSeqExact20( _scriptHash )
    val toScriptPubKey = P2SH.scriptPubKeyFor( _scriptHash.toArray )
    def addressType = P2SH_Mainnet
  }

  /*
   * 
   *  Bech32 / SegWit formats
   * 
   */ 

  private final object SegWitHumanReadablePart {
    val Mainnet = "bc"
    val Testnet = "tb"
  }

  /* P2SH */

  private final object P2WPKH {
    val Version = OP(0)

    val WitnessProgramLen = 20

    val ScriptPubKeyLen = 22

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if ( scriptPubKey.length != ScriptPubKeyLen ) {
        Some( s"A P2WPKH scriptPubKey must be ${ScriptPubKeyLen} bytes long, found ${scriptPubKey.length} bytes: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(0) != Version ) {
        Some( s"The first (version) byte of a P2WPKH scriptPubKey should be OP_0 (${toHex(Version)}), found ${toHex(scriptPubKey(0))}: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(1) != Byte_Hash160Length ) {
        Some( s"The second byte of a P2WPKH scriptPubKey should be the public key hash length ${toHex(Byte_Hash160Length)}, found ${toHex(scriptPubKey(1))}: 0x${scriptPubKey.hex}" )
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
    val encoding = SegWit( P2WPKH.Version, P2WPKH.WitnessProgramLen, P2WPKH.HumanReadablePart.Mainnet )

    def fromPayload( payload : immutable.Seq[Byte] ) : P2WPKH_Mainnet = this.fromPublicKeyHash( ByteSeqExact20( payload ) )

    def fromPublicKeyHash( publicKeyHash : ByteSeqExact20 ) : P2WPKH_Mainnet = this.apply( enc.SegWit.encode(P2WPKH.HumanReadablePart.Mainnet,P2WPKH.Version,publicKeyHash.widen) )

    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2WPKH_Mainnet = {
      P2WPKH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromPublicKeyHash( P2WPKH.extractPublicKeyHash( scriptPubKey ) )
      }
    }
  }
  final case class P2WPKH_Mainnet( val text : String ) extends PublicKeyHashRecoverable {
    val witnessProgram = {
      val ( version, wp ) = enc.SegWit.decode(Some(P2WPKH.HumanReadablePart.Mainnet), text)
      require(
        version == P2WPKH.Version && wp.length == P2WPKH.WitnessProgramLen,
        s"Bad address '${text}': P2WPKH_Mainnet should specify version ${P2WPKH.Version} and a ${P2WPKH.WitnessProgramLen}-byte witness program. Found version ${version} and a ${wp.length} byte witness program."
      )
      wp.toImmutableSeq
    }

    def payload = witnessProgram
    def addressType = P2WPKH_Mainnet

    val toPublicKeyHash = ByteSeqExact20( witnessProgram )
    val toScriptPubKey = {
      val buffer = new mutable.ArrayBuffer[Byte](P2WPKH.ScriptPubKeyLen)
      buffer += P2WPKH.Version
      buffer += Byte_Hash160Length
      buffer ++= witnessProgram
      buffer.toArray.toImmutableSeq
    }
  }

  /* P2WSH */

  private final object P2WSH {
    val Version = OP(0)

    val WitnessProgramLen = 32

    val ScriptPubKeyLen = 34

    private[BtcAddress]
    def whyBadScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : Option[String] = {
      if ( scriptPubKey.length != ScriptPubKeyLen ) {
        Some( s"A P2WSH scriptPubKey must be ${ScriptPubKeyLen} bytes long, found ${scriptPubKey.length} bytes: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(0) != Version ) {
        Some( s"The first (version) byte of a P2WSH scriptPubKey should be OP_0 (${toHex(Version)}), found ${toHex(scriptPubKey(0))}: 0x${scriptPubKey.hex}" )
      }
      else if ( scriptPubKey(1) != Byte32 ) {
        Some( s"The second byte of a P2WSH scriptPubKey should be the payload length ${toHex(Byte32)}, found ${toHex(scriptPubKey(1))}: 0x${scriptPubKey.hex}" )
      }
      else {
        None
      }
    }

    private[BtcAddress]
    def extractScriptHash( scriptPubKey : immutable.Seq[Byte] ) : ByteSeqExact32 = ByteSeqExact32( scriptPubKey.drop(2) )

    val HumanReadablePart = SegWitHumanReadablePart
  }
  final object P2WSH_Mainnet extends Type {
    val encoding = SegWit( P2WSH.Version, P2WSH.WitnessProgramLen, P2WSH.HumanReadablePart.Mainnet )
    def fromPayload( payload : immutable.Seq[Byte] ) : P2WSH_Mainnet = {
      fromPayload( payload.toArray )
    }
    def fromPayload( payload : Array[Byte] ) : P2WSH_Mainnet = {
      require( payload.length == P2WSH.WitnessProgramLen, s"The payload of a P2WSH address should be ${P2WSH.WitnessProgramLen} bytes long, found ${payload.length}: 0x${payload.hex}" )
      val version = P2WSH.Version
      val text = enc.SegWit.encode( P2WSH.HumanReadablePart.Mainnet, version, payload )
      this.apply( text )
    }
    def fromScriptHash ( scriptHash : immutable.Seq[Byte] ) = fromPayload( scriptHash )
    def fromScriptHash ( scriptHash : Array[Byte] )         = fromPayload( scriptHash )
    def fromScriptPubKey( scriptPubKey : immutable.Seq[Byte] ) : P2WSH_Mainnet = {
      P2WSH.whyBadScriptPubKey( scriptPubKey ) match {
        case Some( problem ) => throw new UnexpectedScriptPubKeyFormatException( problem )
        case None            => fromScriptHash( P2WSH.extractScriptHash( scriptPubKey ).widen )
      }
    }
  }
  final case class P2WSH_Mainnet( val text : String ) extends BtcAddress {
    val witnessProgram = {
      val ( version, wp ) = enc.SegWit.decode(Some(P2WSH.HumanReadablePart.Mainnet), text)
      require(
        version == P2WSH.Version && wp.length == P2WSH.WitnessProgramLen,
        s"Bad address '${text}': P2WSH_Mainnet should specify version ${P2WSH.Version} and a ${P2WSH.WitnessProgramLen}-byte witness program. Found version ${version} and a ${wp.length} byte witness program."
      )
      wp.toImmutableSeq
    }
    def payload = witnessProgram
    def addressType = P2WSH_Mainnet
    val toScriptPubKey = {
      val buffer = new mutable.ArrayBuffer[Byte](P2WSH.ScriptPubKeyLen)
      buffer += P2WSH.Version
      buffer += Byte32
      buffer ++= witnessProgram
      buffer.toArray.toImmutableSeq
    }
  }

  sealed trait PublicKeyHashRecoverable extends BtcAddress {
    def toPublicKeyHash : ByteSeqExact20
  }

  def parse( text : String ) : Failable[BtcAddress] = {
    def mainnetSegWitAttempts = Failable {
      val ( version, witnessProgram ) = enc.SegWit.decode( Some(SegWitHumanReadablePart.Mainnet), text )
      ( version, witnessProgram.length ) match {
        case ( 0, P2WPKH.WitnessProgramLen ) => P2WPKH_Mainnet( text ) // we'll decode SegWit redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case ( 0, P2WSH.WitnessProgramLen )  => P2WSH_Mainnet ( text ) // we'll decode SegWit redundantly in the ctor, but trying to avoid that isn't worth the hassle
        case other => throw new UnknownBtcAddressFormatException( s"Couldn't parse '${text}, decodes as SegWit, but unexpected version ${version} and/or witness program length ${witnessProgram.length}: ${other} " )
      }
    }
    def base58Attempts = Failable {
      val ( version, payload ) = enc.Base58.decodeChecked( text )
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
    Failable( P2WSH_Mainnet.fromScriptPubKey( scriptPubKey) )  orElseTrace
    Failable( P2PKH_Mainnet.fromScriptPubKey( scriptPubKey ) ) orElseTrace
    Failable( P2SH_Mainnet.fromScriptPubKey( scriptPubKey ) )
  }

  // throws exceptions on failure
  def apply( text : String ) : BtcAddress = parse( text ).assert
}
sealed trait BtcAddress {
  def text            : String
  def payload         : immutable.Seq[Byte]
  def addressType     : BtcAddress.Type
  def toScriptPubKey  : immutable.Seq[Byte]
}
