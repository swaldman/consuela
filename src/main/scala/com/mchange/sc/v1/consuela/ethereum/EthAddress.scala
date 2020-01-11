/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum;

import com.mchange.sc.v1.consuela._;
import encoding.{RLP,Nibble}

import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact20, Unsigned256}

import scala.collection._

object EthAddress {
  final object BadMixedCaseChecksumException {
    private def message( mixedCaseHex : String, mbChainId : Option[EthChainId] ) = {
      val main = s"The checksum embedded in the mixed case of '${mixedCaseHex}' is incorrect for this address"
      val suffix = {
        mbChainId match {
          case Some( chainId ) => main + s" and chain ID ${chainId.value} (under RSKIP-60)."
          case None            => " (under EIP-55)."
        }
      }
      main + suffix
    }
  }
  final class BadMixedCaseChecksumException( mixedCaseHex : String, mbChainId : Option[EthChainId] ) extends EthereumException( BadMixedCaseChecksumException.message( mixedCaseHex, mbChainId ) )

  // Implements https://github.com/rsksmart/RSKIPs/blob/master/IPs/RSKIP60.md
  final object RSKIP60 {
    import java.nio.charset.StandardCharsets.US_ASCII

    def toChecksumHex( address : EthAddress, mbChainId : Option[EthChainId] ) : String = {
      // careful not to call address.hex, which would be an endless recursion
      val unprefixedLowercaseHex = address.bytes.widen.hex.toLowerCase
      assert( !unprefixedLowercaseHex.startsWith("0x"), s"address.bytes.widen.hex unexpectedly yields 0x-prefixed hex string:'${unprefixedLowercaseHex}'" )
      val hashable = {
        val s = mbChainId match {
          case Some( chainId ) => chainId.value.widen + "0x" + unprefixedLowercaseHex
          case None            => unprefixedLowercaseHex
        }
        s.getBytes()
      }
      val hashHex    = EthHash.hash(hashable).hex 
      val outSeq = {
        unprefixedLowercaseHex.zip( hashHex ).map { case ( sourceDigit, checkDigit ) =>
          if ( Integer.parseInt( String.valueOf(checkDigit), 16 ) >= 8 ) Character.toUpperCase(sourceDigit) else sourceDigit
        }
      }
      outSeq.mkString
    }
  }

  final object EIP55 {
    // implements https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md
    def toChecksumHex( address : EthAddress ) : String = RSKIP60.toChecksumHex( address, None )
  }

  val ByteLength = 20;

  def apply( pub : EthPublicKey ) : EthAddress = new EthAddress( this.computeBytes( pub ) );

  def computeBytes( pub : EthPublicKey ) : ByteSeqExact20 = ByteSeqExact20( EthHash.hash(pub.bytes.widen).toByteArray.drop(12) );

  private def _parse( hexString : String, mbChainId : Option[EthChainId] ) : Option[EthAddress] = {
    val addr = EthAddress( ByteSeqExact20( hexString.decodeHex ) )
    val deprefixed = if ( hexString.startsWith("0x") ) hexString.drop(2) else hexString
    if ( deprefixed.isMixedCase && deprefixed != RSKIP60.toChecksumHex( addr, mbChainId ) ) None else Some(addr)
  }

  def parse( hexString : String, mbChainId : Option[EthChainId], withOrWithoutChainIdIfPresent : Boolean ) : EthAddress = {
    val out = {
      if ( withOrWithoutChainIdIfPresent && mbChainId.nonEmpty ) {
        _parse( hexString, mbChainId ) orElse _parse( hexString, None )
      }
      else {
        _parse( hexString, mbChainId )
      }
    }
    out.getOrElse( throw new BadMixedCaseChecksumException( hexString, mbChainId ) )
  }

  def parseStrictly( hexString : String, chainId : EthChainId ) : EthAddress = {
    val chain = Some( chainId )
    this.parse( hexString, chain, false )
  }

  def parseStrictly( hexString : String, mbChainId : Option[EthChainId] ) : EthAddress = {
    parse( hexString, mbChainId, false )
  }

  def parsePermissively( hexString : String, chainId : EthChainId ) : EthAddress = {
    val chain = Some( chainId )
    this.parse( hexString, chain, true )
  }

  def parsePermissively( hexString : String, mbChainId : Option[EthChainId] ) : EthAddress = {
    parse( hexString, mbChainId, true )
  }

  def parse( hexString : String ) : EthAddress = this.parse( hexString, None, true )

  def apply( hexString : String ) : EthAddress = this.parse( hexString )

  def apply( bytes : Array[Byte] ) : EthAddress = EthAddress( ByteSeqExact20( bytes ) )

  def apply( bytes : Seq[Byte] ) : EthAddress = EthAddress( ByteSeqExact20( bytes ) )

  def forContract( creator : EthAddress, nonce : Unsigned256 ) : EthAddress = {  // see https://ethereum.stackexchange.com/questions/760/how-is-the-address-of-an-ethereum-contract-computed
    val seq = RLP.Element.Seq.of( RLP.toElement(creator), RLP.toElement(nonce) )
    val seqBytes = RLP.Element.encode( seq )
    val hashBytes = EthHash.hash( seqBytes ).bytes
    EthAddress( hashBytes.drop(12) )
  }

  val Zero = EthAddress( ByteSeqExact20( Array.fill[Byte](20)(0.toByte) ) )

  trait Source[-T] {
    /**
      * May throw if the instance of T cannot be converted
      */ 
    def toEthAddress( t : T ) : EthAddress
  }
  implicit final object EthAddressIsSource extends EthAddress.Source[EthAddress]{
    def toEthAddress( address : EthAddress ) = address
  }
  implicit final object StringIsSource extends EthAddress.Source[String]{
    def toEthAddress( hex : String ) = EthAddress( hex )
  }
  implicit final object ByteSeqIsSource extends EthAddress.Source[Seq[Byte]]{
    def toEthAddress( seq : Seq[Byte] ) = EthAddress( seq )
  }
  implicit final object ImmutableByteSeqIsSource extends EthAddress.Source[immutable.Seq[Byte]]{
    def toEthAddress( seq : immutable.Seq[Byte] ) = EthAddress( seq )
  }
  implicit final object ByteArrayIsSource extends EthAddress.Source[Array[Byte]]{
    def toEthAddress( arr : Array[Byte] ) = EthAddress( arr )
  }
  implicit final object ByteSeqExact20IsSource extends EthAddress.Source[ByteSeqExact20]{
    def toEthAddress( bytes : ByteSeqExact20 ) = EthAddress( bytes )
  }
}
final case class EthAddress( val bytes : ByteSeqExact20 ) {
  lazy val toNibbles : IndexedSeq[Nibble] = encoding.toNibbles( bytes.widen )

  def hex = eip55Hex

  def hex0x = "0x" + hex

  def eip55Hex = EthAddress.EIP55.toChecksumHex( this )

  def rskip60HexForChainId( mbChainId : Option[EthChainId] ) : String = EthAddress.RSKIP60.toChecksumHex( this, mbChainId )

  def rskip60HexForChainId( chainId : EthChainId ) : String = EthAddress.RSKIP60.toChecksumHex( this, Some( chainId ) )

  def matches( pub : EthPublicKey ) : Boolean = bytes == EthAddress.computeBytes( pub );
}
