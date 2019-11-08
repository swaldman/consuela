package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.crypto.secp256k1.decompressPublicKey
import com.mchange.sc.v1.consuela.ethereum.EthPublicKey
import com.mchange.sc.v1.consuela.hash.{RIPEMD160,SHA256}

import com.mchange.sc.v3.failable._

import scala.collection._

object BtcPublicKey {
  def fromPrivateKey( privateKey : BtcPrivateKey ) : BtcPublicKey = this.apply( privateKey.toEthPrivateKey.toPublicKey )

  def fromRawBytes( bytes64 : ByteSeqExact64 ) : BtcPublicKey = this.apply( EthPublicKey( bytes64 ) )

  def fromUncompressedBytes( bytes65 : ByteSeqExact65 ) : BtcPublicKey = {
    EthPublicKey.fromBytesWithUncompressedHeader( bytes65 ) match {
      case Succeeded( epk ) => this.apply( epk )
      case oops : Failed[_] => throw new UnknownPublicKeyFormatException( s"Could not parse 65 byte uncompressed public key", oops.toThrowable )
    }
  }

  def fromCompressedBytes( bytes33 : ByteSeqExact33 ) : BtcPublicKey = {
    val raw = bytes33.widen
    val header = raw.head
    val x = raw.tail.toUnsignedBigInt
    this.fromUncompressedBytes( ByteSeqExact65( decompressPublicKey( x.bigInteger, header ) ) )
  }

  def apply( bytes : immutable.Seq[Byte] ) : BtcPublicKey = {
    bytes.length match {
      case 33    => this.fromCompressedBytes( ByteSeqExact33( bytes ) )
      case 64    => this.fromRawBytes( ByteSeqExact64( bytes ) )
      case 65    => this.fromUncompressedBytes( ByteSeqExact65( bytes ) )
      case other => throw new UnknownPublicKeyFormatException( s"Unexpected length for public key in any format : ${other}" )
    }
  }

  def apply( bytes : Array[Byte] ) : BtcPublicKey = this.apply( bytes.toImmutableSeq )

  final object Header {
    val CompressedEven = 0x02.toByte
    val CompressedOdd  = 0x03.toByte
    val Uncompressed   = 0x04.toByte
  }

  private val CompressedLength = 33
  private val CoordinateLength = 32
}
case class BtcPublicKey( val toEthPublicKey : EthPublicKey ) {
  import BtcPublicKey._

  private def hash( bytes : immutable.Seq[Byte] ) : ByteSeqExact20 = ByteSeqExact20( RIPEMD160.hash( SHA256.hash( bytes ).bytes ).bytes )

  def headerlessBytes = toEthPublicKey.bytes
  def headerlessHex = toEthPublicKey.bytes.widen.hex

  // includes header byte 0x04
  def uncompressedBytes     = toEthPublicKey.bytesWithUncompressedHeader
  lazy val uncompressedHex  = toEthPublicKey.bytesWithUncompressedHeader.widen.hex
  lazy val uncompressedHash = hash( uncompressedBytes.widen )

  // includes header byte 0x02 or 0x03
  lazy val compressedBytes = {
    val x = toEthPublicKey.x
    val y = toEthPublicKey.y
    val buff = new mutable.ArrayBuffer[Byte](CompressedLength)
    buff += ( if ( y % 2 == 0 ) Header.CompressedEven else Header.CompressedOdd )
    buff ++= x.unsignedBytes( CoordinateLength )
    ByteSeqExact33( buff.toArray.toImmutableSeq )
  }
  lazy val compressedHex  = compressedBytes.widen.hex
  lazy val compressedHash = hash( compressedBytes.widen )

  def toBtcAddress( ofType : BtcAddress.Type = BtcAddress.P2PKH_Mainnet, useCompressedKey : Boolean = true ) : BtcAddress = {
    val hash = if ( useCompressedKey ) compressedHash else uncompressedHash


    ofType match {
      case BtcAddress.P2PKH_Mainnet => BtcAddress.P2PKH_Mainnet.fromPublicKeyHash( hash )
      case _ => throw new UnsupportedBtcAddressTypeException( s"Conversion of public keys to ${ofType} addresses is not currently supported." )
    }
  }

  override def toString : String = s"BtcPublicKey(0x${uncompressedHex})"
}
