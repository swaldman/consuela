package com.mchange.sc.v1.consuela.bitcoin

import encoding.Base58

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.EthPrivateKey

object BtcPrivateKey {
  def fromHex( privateKeyHex : String ) : BtcPrivateKey = new BtcPrivateKey( new EthPrivateKey( ByteSeqExact32( privateKeyHex.decodeHex ) ) )

  def fromBase64( base64String : String ) : BtcPrivateKey = new BtcPrivateKey( new EthPrivateKey( ByteSeqExact32( Base64Decoder.decode( base64String ) ) ) )

  val CompressedMarker = 0x01.toByte

  final object Header {
    val Mainnet = 0x80.toByte
    val Testnet = 0xef.toByte
  }

  // javadoc: "Instances of Base64.Encoder class are safe for use by multiple concurrent threads."
  private lazy val Base64Encoder = java.util.Base64.getEncoder()

  // javadoc: "Instances of Base64.Decoder class are safe for use by multiple concurrent threads."
  private lazy val Base64Decoder = java.util.Base64.getDecoder()
}
final case class BtcPrivateKey private ( val toEthPrivateKey : EthPrivateKey )  {
  import BtcPrivateKey._

  private [this] lazy val raw = toEthPrivateKey.bytes.widen.toArray

  def hex = toEthPrivateKey.hex

  lazy val base64 = Base64Encoder.encodeToString( raw )

  def wif( mainnet : Boolean = true, compressed : Boolean = true ) : String = {
    val header  = if ( mainnet ) Header.Mainnet else Header.Testnet
    val payload = if ( compressed ) raw :+ CompressedMarker else raw
    Base58.encodeChecked( header, payload )
  }

  lazy val toPublicKey : BtcPublicKey = BtcPublicKey( toEthPrivateKey.toPublicKey )

  override def toString() : String = "BtcPrivateKey(ByteSeqExact32(<masked>))"
}
