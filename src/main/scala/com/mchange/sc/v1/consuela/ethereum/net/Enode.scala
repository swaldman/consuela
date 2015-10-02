package com.mchange.sc.v1.consuela.ethereum.net;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum.EthPublicKey;
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact64,Unsigned16};

object Enode {
  val ParseRegex = """enode\:\/\/(\p{XDigit}{128})\@([\d\.\:]+)\:(\d+)(?:\?discport\=(\d+))?""".r

  // an unspecified discovery port defaults to the main (TCP) port
  def apply( nodeId : EthPublicKey, address : IPAddress, port : Unsigned16 ) : Enode = Enode( nodeId, address, port, port );

  def apply( urlStr : String ) : Enode = {
    urlStr match {
      case ParseRegex( id, ip, port, discport ) => {
        val discoveryPort = if ( discport == null ) port else discport
        Enode( EthPublicKey( id.decodeHex ), IPAddress( ip ), Unsigned16( port.toInt ), Unsigned16( discoveryPort.toInt ) )
      }
      case _ => throw new java.net.MalformedURLException( s"'${urlStr}' is not a valid enode URL." )
    }
  }
}
final case class Enode( nodeId : EthPublicKey, address : IPAddress, port : Unsigned16, discoveryPort : Unsigned16 ) {
  override lazy val toString : String = {
    val suffix = if ( port != discoveryPort ) "?discport=${discoveryPort}" else ""
    s"enode://${nodeId.hex}@${address}:${port}${suffix}"
  }
}

