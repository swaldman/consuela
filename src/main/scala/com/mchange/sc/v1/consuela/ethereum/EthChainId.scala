package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v2.restrict.RestrictedInt.UnsignedInt
import specification.Types.{SignatureV,SignatureWithChainIdV}


/**
  * Represents an EIP 155 Chain ID, with its (optional) name
  * 
  * See https://eips.ethereum.org/EIPS/eip-155
  */
object EthChainId {
  val Mainnet    = EthChainId( 1, Some("mainnet"))
  val Ropsten    = EthChainId( 3, Some("ropsten"))
  val Rinkeby    = EthChainId( 4, Some("rinkeby"))
  val Kovan      = EthChainId(42, Some("kovan"))
  val EtcMainnet = EthChainId(61, Some("eth-classic-mainnet"))
  val EtcTestnet = EthChainId(62, Some("eth-classic-testnet"))

  val Known = Mainnet :: Ropsten :: Rinkeby :: Kovan :: EtcMainnet :: EtcTestnet :: Nil

  def findKnown( value : Int ) : Option[EthChainId] = Known.find( _.value.widen == value )

  def apply( value : Int, name : Option[String] = None ) : EthChainId = {
    name match {
      case someName : Some[String] => new EthChainId( UnsignedInt(value), someName )
      case None                    => EthChainId.findKnown( value ).getOrElse( new EthChainId(UnsignedInt(value), None ) )
    }
  }

  def extract( v : SignatureWithChainIdV ) : ( SignatureV, EthChainId ) = {
    val _v = v.widen
    val ( sigv, idval ) = if ((_v % 2) == 0) ( SignatureV(28), (_v - 36) / 2 ) else ( SignatureV(27), (_v - 35) / 2 )
    ( sigv, EthChainId( idval ) )
  }
}
final class EthChainId private ( val value : UnsignedInt, val name : Option[String] ){

  def signatureWithChainIdV( rawSignatureV : Int ) : SignatureWithChainIdV = {
    rawSignatureV match {
      case 27 => SignatureWithChainIdV( (value.widen * 2) + 35 )
      case 28 => SignatureWithChainIdV( (value.widen * 2) + 36 )
      case _  => throw new IllegalArgumentException( "Raw signature V must be either 27 or 28." )
    }
  }
  def signatureWithChainIdV( signatureV : SignatureV ) : SignatureWithChainIdV = signatureWithChainIdV( signatureV.widen )

  /**
    * Chain IDs are equal if their values are equal.
    * The optional name is just a convenience.
    */ 
  override def equals( that : Any ) : Boolean = {
    that match {
      case eci : EthChainId => this.value == eci.value
      case _                => false
    }
  }
  override def hashCode() : Int = this.value.widen

  override def toString() : String = {
    name match {
      case Some( nm ) => s"""EthChainId( ${value.widen}, "${name.get}" )"""
      case None       => s"""EthChainId( ${value.widen} )"""
    }
  }
}
