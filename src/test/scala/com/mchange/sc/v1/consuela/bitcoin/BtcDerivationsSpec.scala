package com.mchange.sc.v1.consuela.bitcoin

import scala.collection._

import org.specs2._

import com.mchange.sc.v1.consuela._

object BtcDerivationSpec {
  case class WalletRec(
    privateKeyHex             : String,
    privateKeyBase64          : Option[String] = None,
    privateKeyWifUncompressed : Option[String] = None,
    privateKeyWifCompressed   : Option[String] = None,
    publicKeyUncompressedHex  : Option[String] = None,
    publicKeyCompressedHex    : Option[String] = None,
    p2pkhMainnetUncompressed  : Option[String] = None,
    p2pkhMainnetCompressed    : Option[String] = None
  )
  val WalletRecs = immutable.Seq ( // data generated at https://www.bitaddress.org/
    WalletRec(
      privateKeyHex             = "1579638E1A0A035B7EDE21A2133B73CB843034C03B6A5D47D57BD164B5C69FEA",
      privateKeyBase64          = Some("FXljjhoKA1t+3iGiEztzy4QwNMA7al1H1XvRZLXGn+o="),
      privateKeyWifUncompressed = Some("5Hyk7Mxvvg6GnSY4RCoC8AYWuY9GpqoFHEx2wfzZfdR3gRnmrY8"),
      privateKeyWifCompressed   = Some("KwwTH5Xs1uYGfZi65q4t6d86ztTWrAP82gZuaK75BVj8pzio2H5p"),
      publicKeyUncompressedHex  = Some("048C0F34D75578EBDA262D12D7591CE8EC02F4C34296A75F45F9550435CA330FCA6BA0BCA10E5E1830EC7DECEA4804DA8CAD4DD71E4040E8D237AC8E9291F823A0"),
      publicKeyCompressedHex    = Some("028C0F34D75578EBDA262D12D7591CE8EC02F4C34296A75F45F9550435CA330FCA"),
      p2pkhMainnetUncompressed  = Some("13qniDXuHXZpcd1RXT3S92GNC526iH4ior"),
      p2pkhMainnetCompressed    = Some("1FfT18jEtRhqnCsMyu4EicCK5GRb59N98H")
    ),
    WalletRec(
      privateKeyHex             = "86FD9E4EDA05C8154ADA63E500FA0B81C14843E77E3C696F8260ECAEB6E4E4CD",
      privateKeyBase64          = Some("hv2eTtoFyBVK2mPlAPoLgcFIQ+d+PGlvgmDsrrbk5M0="),
      privateKeyWifUncompressed = Some("5JqjjjpXFsD77JKfDaNmJHxyZPdTiwV6fn7oYC6UTLchEyzDuHe"),
      privateKeyWifCompressed   = Some("L1k7ddzkYccRxoTMgWK4XKt4dxsvx7V22A92QfyABFaYcS8Y58Qa"),
      publicKeyUncompressedHex  = Some("04A63D62709EDECE67D59B7E97D8F65A4E8768D1CEF6FC34A8A2F59822250DE94BEC4559C8A7111EC0E6495CB1546BBA68262081FF417A096261EF4D0E93861C6F"),
      publicKeyCompressedHex    = Some("03A63D62709EDECE67D59B7E97D8F65A4E8768D1CEF6FC34A8A2F59822250DE94B"),
      p2pkhMainnetUncompressed  = Some("1KzMwUALZKmUGAD4cAgzYfiC5TsYnnA8UG"),
      p2pkhMainnetCompressed    = Some("1G9E1TeYDEoVvxvkPLmipR4UtRu7RmW4VA")
    )
  )

  val TextAddressesToScriptPubKeys = immutable.Map( // taken from https://eips.ethereum.org/EIPS/eip-2304
    "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa" -> "76a91462e907b15cbf27d5425399ebf6f0fb50ebb88f1888ac",
    "3Ai1JZ8pdJb2ksieUV8FsxSNVJCpoPi8W6" -> "a91462e907b15cbf27d5425399ebf6f0fb50ebb88f1887"
  )

  private def bad( msg : => String ) : Boolean = {
    println( s"BAD: ${msg}" )
    false
  }

  def privateKeyFormatsOK( rec : WalletRec ) : Boolean = {
    val bpkHex = BtcPrivateKey.fromHex( rec.privateKeyHex )

    def generatesSameBase64     = rec.privateKeyBase64.fold( true ){ bpkBase64 => ( bpkHex.base64 == bpkBase64 || bad( s"from hex generated base64 and given base 64 not equal: ${rec}" ) ) }
    def equalsViaBase64         = rec.privateKeyBase64.fold( true ){ bpkBase64 => ( bpkHex == BtcPrivateKey.fromBase64( bpkBase64 ) || bad( s"from hex and from base 64 keys not equal: ${rec}" ) ) }

    def expectedWifUncompressed = rec.privateKeyWifUncompressed.fold( true ){ expected => ( bpkHex.wif( compressed = false ) == expected || bad( s"wif uncompressed not expected: ${rec}" ) ) }
    def expectedWifCompressed   = rec.privateKeyWifCompressed.fold( true )  { expected => ( bpkHex.wif( compressed = true )  == expected || bad( s"wif compressed not expected: ${rec}" ) ) }

    generatesSameBase64 && equalsViaBase64 && expectedWifUncompressed && expectedWifCompressed
  }
  def expectedPublicKeys( rec : WalletRec ) : Boolean = {
    val bpkHex           = BtcPrivateKey.fromHex( rec.privateKeyHex )
    val derivedPublicKey = bpkHex.toPublicKey


    def generatesSamePublicKeyUncompressed = rec.publicKeyUncompressedHex.fold( true ){ upubhex =>
      ( derivedPublicKey == BtcPublicKey(upubhex.decodeHex) || bad( s"from uncompressed hex generated pubkey+derived pubkey not equal: ${rec}" ) )
    }
    def generatesSamePublicKeyCompressed = rec.publicKeyCompressedHex.fold( true ){ cpubhex =>
      ( derivedPublicKey == BtcPublicKey(cpubhex.decodeHex) || bad( s"from compressed hex generated pubkey+derived pubkey not equal: ${rec}" ) )
    }

    def derivedPublicKeyUncompressed = rec.publicKeyUncompressedHex.fold( true ){ expected => ( derivedPublicKey.uncompressedHex.equalsIgnoreCase(expected) || bad( s"derived uncompessed pubkey hex != expected: ${rec}" ) ) }
    def derivedPublicKeyCompressed = rec.publicKeyCompressedHex.fold( true ){ expected => ( derivedPublicKey.compressedHex.equalsIgnoreCase(expected) || bad( s"derived uncompessed pubkey hex != expected: ${rec}" ) ) }

    generatesSamePublicKeyUncompressed && generatesSamePublicKeyCompressed && derivedPublicKeyUncompressed && derivedPublicKeyCompressed
  }
  def expectedAddresses( rec : WalletRec ) : Boolean = {
    val bpkHex           = BtcPrivateKey.fromHex( rec.privateKeyHex )
    val derivedPublicKey = bpkHex.toPublicKey

    val derivedAddressUncompressed = derivedPublicKey.toBtcAddress( ofType = BtcAddress.P2PKH_Mainnet, useCompressedKey = false )
    val derivedAddressCompressed   = derivedPublicKey.toBtcAddress( ofType = BtcAddress.P2PKH_Mainnet, useCompressedKey = true )

    def generatesAddressUncompressed = rec.p2pkhMainnetUncompressed.fold( true ){ address =>
      ( derivedAddressUncompressed == BtcAddress.parse(address).assert || bad( s"derived and expected address not equal: ${rec}" ) )
    }
    def generatesAddressCompressed = rec.p2pkhMainnetCompressed.fold( true ){ address =>
      ( derivedAddressCompressed == BtcAddress.parse(address).assert || bad( s"derived and expected address not equal: ${rec}" ) )
    }

    generatesAddressUncompressed && generatesAddressCompressed
  }

}
class BtcDerivationSpec extends Specification { def is = s2"""
   BTC private key formats derive and interchange               ${e1} 
   BTC public keys derive as expected                           ${e2} 
   BTC addresses derive as expected                             ${e3} 
   scriptPubKeys are as expected                                ${e4}
"""

  import BtcDerivationSpec._

  def e1 : Boolean = WalletRecs.map( privateKeyFormatsOK ).forall( identity )
  def e2 : Boolean = WalletRecs.map( expectedPublicKeys ).forall( identity )
  def e3 : Boolean = WalletRecs.map( expectedAddresses ).forall( identity )
  def e4 : Boolean = TextAddressesToScriptPubKeys.map { case ( textAddress, spkHex ) => BtcAddress.parse( textAddress ).assert.toScriptPubKey == spkHex.decodeHexAsSeq }.forall( identity ) 
}
