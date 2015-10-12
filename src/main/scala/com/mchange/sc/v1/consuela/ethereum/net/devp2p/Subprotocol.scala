package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable
import com.mchange.sc.v2.failable._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._

object Subprotocol {
  /*
  final object Name {
    val Core = "!core"; // exclamation point so that Core's name comes first in standard orderings
    val Eth  = "eth";
    val Shh  = "shh";
  }
  */ 

  val All : immutable.IndexedSeq[Subprotocol] = immutable.IndexedSeq( Core );

  val byIdentifier : immutable.Map[ (String, Int), Subprotocol ] = immutable.Map( All.map( sp => sp.Identifier -> sp ) : _* )

  abstract class Base( val Name : String, val Version : Unsigned16 ) extends Subprotocol {

    val PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]]; // we can't just put this in the constructor, alas, because we need to refer to PayloadFactories our subclass will contain
  }

  // note the exclamation point so that Core's name comes first when names are ordered with the default string ordering
  // there is no standard name for the core subprotocol, so this hack is fine
  object Core extends Subprotocol.Base( "!core", Unsigned16(0) ) {
    final object Hello extends Payload.Factory.Base[Hello]( this ){
      case class Capabilities( elements : immutable.Set[(StringASCII_Exact3, Unsigned16)] );
    }
    final case class Hello(
      typeCode     : Unsigned16, 
      p2pVersion   : Unsigned16, 
      clientId     : StringUTF8, 
      capabilities : Hello.Capabilities,
      listenPort   : Unsigned16, 
      nodeId       : ByteSeqExact64 
    ) extends Payload.Base[Hello]( Hello );

    final object Disconnect extends Payload.Factory.Base[Disconnect]( this ){
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
    final case class Disconnect( typeCode : Unsigned16, reason : Unsigned16 ) extends Payload.Base[Disconnect]( Disconnect ) { 
      override def toString : String = super.toString + s" [${Disconnect.ReasonMessages( reason.widen )}]" 
    }

    final object Ping extends Payload.Factory.Base[Ping]( this );
    final case class Ping( typeCode : Unsigned16 ) extends Payload.Base[Ping]( Ping );

    final object Pong extends Payload.Factory.Base[Pong]( this );
    final case class Pong( typeCode : Unsigned16 ) extends Payload.Base[Pong]( Pong );

    // touching this suggests an error has occurred
    final object NoFactory extends Payload.Factory[Nothing] {
      def subprotocol                   : Subprotocol = Core

      def pickle( payload : Nothing )   : immutable.Seq[Byte] = throw new RuntimeException("It should be impossible even to provoke this Exception")
      def unpickle( bytes : Seq[Byte] ) : Failable[Nothing] = fail("Tried to unpickle a payload with a bad typecode.")

      override lazy val offset : Unsigned16 = throw new RuntimeException("Core.NoFactory represents no valid offset.")
    }


    //NOTE: This sequence defined the offsets within the subprotocol!
    lazy val PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]] = {
      val defined = immutable.IndexedSeq( Core.Hello, Core.Disconnect, Core.Ping, Core.Pong ).lift;

      def factoryForIndex( i : Int ) : Payload.Factory[_] = defined( i ).getOrElse( Core.NoFactory )

      (0x00 until 0x10).map( factoryForIndex )
    }
  }

//  object Eth {
//  }
  
}
trait Subprotocol {
  def Name : String;
  def Version : Unsigned16;
  def PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]];

  lazy val Identifier : ( String, Int ) = ( Name, Version.widen )
}
