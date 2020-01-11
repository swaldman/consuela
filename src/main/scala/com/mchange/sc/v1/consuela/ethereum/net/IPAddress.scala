package com.mchange.sc.v1.consuela.ethereum.net;

import scala.collection._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact4,ByteSeqExact16}

object IPAddress{
  val RawAddressRegex = """^[\p{XDigit}\.\:]+$""".r //very loose, but should exclude most hostnames

  def apply( bytes : Seq[Byte] ) : IPAddress = apply( bytes.toImmutableSeq )

  def apply( bytes : immutable.Seq[Byte] ) : IPAddress = {
    bytes.length match {
      case 4   => IPv4Address( ByteSeqExact4( bytes ) )
      case 16  => IPv6Address( ByteSeqExact16( bytes ) )
      case bad => throw new IllegalArgumentException( s"Unknown IP Address format, expected 4 or 16 bytes, found ${bad}." )
    }
  }
  def apply( formatted : String ) : IPAddress = {
    formatted match {
      case RawAddressRegex() => {
        import java.net._

        val jAddr = InetAddress.getByName( formatted );
        jAddr match {
          case ip4 : Inet4Address => IPv4Address( ByteSeqExact4( ip4.getAddress().toImmutableSeq ) );
          case ip6 : Inet6Address => IPv6Address( ByteSeqExact16( ip6.getAddress().toImmutableSeq ) );
          case bad => throw new Exception( s"Unknown address type found: ${bad}" );
        }
      }
      case bad => throw new IllegalArgumentException("Only raw IP addresses are acceptable. '${bad}' is not a raw IP.");
    }
  }
  def parse( formatted : String ) : IPAddress = this.apply( formatted )

  def parseV4( formatted : String ) : IPv4Address = {
    this.parse( formatted ) match {
      case v4 : IPv4Address => v4
      case v6 : IPv6Address => throw new IllegalArgumentException("'${v6}' is not an IPv4 address, but an IPv6Address.");
    }
  }

  def parseV6( formatted : String ) : IPv6Address = {
    this.parse( formatted ) match {
      case v4 : IPv4Address => throw new IllegalArgumentException("'${v4}' is not an IPv6 address, but an IPv4Address."); 
      case v6 : IPv6Address => v6
    }
  }
}
sealed trait IPAddress {
  def toUrlHost : String
}
final object IPv4Address {
  def apply( formatted : String ) : IPv4Address = IPAddress.parseV4( formatted )
}
final case class IPv4Address( bytes : ByteSeqExact4 ) extends IPAddress {
  override lazy val toString : String = bytes.widen.map( _ & 0xFF ).mkString(".")
  def toUrlHost : String = this.toString
}
final object IPv6Address {
  def apply( formatted : String ) : IPv6Address = IPAddress.parseV6( formatted )
}
final case class IPv6Address( bytes : ByteSeqExact16 ) extends IPAddress {
  override lazy val toString : String = bytes.widen.grouped(2).map( seq => (seq(0) << 8 | seq(1)).toHexString ).mkString(":")
  def toUrlHost : String = "[" + this.toString + "]"
}
