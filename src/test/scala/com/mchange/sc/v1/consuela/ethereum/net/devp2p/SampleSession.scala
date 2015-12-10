package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import java.io._
import java.net._
import scala.collection.immutable
import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.net.devp2p._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

/*
object SampleSession {
  def main( argv : Array[String] ) : Unit = {
    val host = argv(0)
    val port = argv(1).toInt

    def echo( is : InputStream ) : Thread = {
      val t = new Thread {
        override def run : Unit = {
          var b = is.read
          while ( b >= 0 ) {
            print( Integer.toString(b, 16) )
            b = is.read
          }
          println("Connection closed.")
        }
      }
      t.start
      t
    }

    val EthKeyPair( eprivkey, epubkey ) = EthKeyPair( new java.security.SecureRandom )

    val initiator = Handshake.Message.Initiator( None, epubkey )

    val capabilities = Subprotocol.P2P4.Hello.Capabilities( immutable.Set( Subprotocol.Eth60.Identifier, Subprotocol.P2P4.Identifier ) )
    val hello = Subprotocol.P2P4.Hello( Unsigned16(0), Unsigned16(1), StringUTF8("Test"), capabilities, Unsigned16(0), ByteSeqExact64( epubkey.bytes ) )

    // println( encoding.RLP.toElement( hello ) )

    val helloPacketBytes = Packet.encode( hello )( Session.Bootstrap ).get.toArray

    val s  = new Socket( host, port )

    println( s"Connection established to host '${host}', port ${port}" )

    val is = s.getInputStream
    val os = s.getOutputStream

    val t = echo( is )

    println( s"Writing... ${helloPacketBytes.hex}" )

    os.write( helloPacketBytes )
    os.flush

    t.join
  }
}
*/
