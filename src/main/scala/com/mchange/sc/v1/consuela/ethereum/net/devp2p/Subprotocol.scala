package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable
import com.mchange.sc.v2.failable._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

object Subprotocol {
  final object Name {
    val Base = "!base"; // exclamation point so that Base's name comes first in standard orderings
    val Eth  = "eth";
    val Shh  = "shh";
  }

  final object Info {
    // this is a placeholder, until we've actually implemented some payloads.
    val Base0 : immutable.IndexedSeq[Info] = (0x00 until 0x10).map( i => Info( Name.Base, 0, i, _ => throw new Exception, _ => throw new Exception ) )
  }
  final case class Info( name : String, version : Int, offset : Int, pickle : Payload => Failable[immutable.Seq[Byte]], unpickle : Seq[Byte] => Payload )

  val Known : Map[ (String, Int), immutable.IndexedSeq[Info] ] = immutable.Map(
    ( Name.Base, 0 ) -> Info.Base0
  )

  val BaseTuple = ( Name.Base, 0 )

  object Base {
    final object Hello {
      case class Capabilities( elements : immutable.Set[(StringASCII_Exact3, Unsigned16)] );
    }
    final object Disconnect {
      val ReasonMessages = Map[Int,String] (
        0x00 -> "Disconnect requested",
        0x01 -> "TCP sub-system error",
        0x02 -> "Breach of protocol, e.g. a malformed message, bad RLP, incorrect magic number &c.",
        0x03 -> "Useless peer",
        0x04 -> "Too many peers",
        0x05 -> "Already connected",
        0x06 -> "Incompatible P2P protocol version",
        0x07 -> "Null node identity received - this is automatically invalid",
        0x08 -> "Client quitting",
        0x09 -> "Unexpected identity (i.e. a different identity to a previous connection/what a trusted peer told us).",
        0x0a -> "Identity is the same as this node (i.e. connected to itself)",
        0x0b -> "Timeout on receiving a message (i.e. nothing received since sending last ping)",
        0x10 -> "Some other reason specific to a subprotocol."
      )
    }

    final case class Hello(
      typeCode     : Unsigned16, 
      p2pVersion   : Unsigned16, 
      clientId     : StringUTF8, 
      capabilities : Hello.Capabilities,
      listenPort   : Unsigned16, 
      nodeId       : ByteSeqExact64 
    ) extends Payload {
      override def offset = Unsigned16( 0x00 )
    }
    final case class Disconnect( typeCode : Unsigned16, reason : Unsigned16 ) extends Payload { 
      override def offset = Unsigned16( 0x01 )
      override def toString : String = super.toString + s" [${Disconnect.ReasonMessages( reason.widen )}]" 
    }
    final case class Ping( typeCode : Unsigned16 ) extends Payload {
      override def offset = Unsigned16( 0x02 )
    }
    final case class Pong( typeCode : Unsigned16 ) extends Payload {
      override def offset = Unsigned16( 0x03 )
    }
  }
}
