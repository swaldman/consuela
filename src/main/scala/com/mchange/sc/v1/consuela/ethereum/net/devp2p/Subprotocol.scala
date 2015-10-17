package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable
import com.mchange.sc.v2.failable._

import com.mchange.sc.v1.consuela.ethereum._
import com.mchange.sc.v1.consuela.ethereum.specification.Types._
import com.mchange.sc.v1.consuela.ethereum.encoding.RLPSerializing;

object Subprotocol {
  /*
  final object Name {
    val Core = "!core"; // exclamation point so that Core's name comes first in standard orderings
    val Eth  = "eth";
    val Shh  = "shh";
  }
  */ 

  val All : immutable.IndexedSeq[Subprotocol] = immutable.IndexedSeq( P2P4, Eth60 );

  val byIdentifier : immutable.Map[ (StringASCII_Exact3, Unsigned16), Subprotocol ] = immutable.Map( All.map( sp => sp.Identifier -> sp ) : _* )

  abstract class Base( val Name : StringASCII_Exact3, val Version : Unsigned16 ) extends Subprotocol {
    // we can't just put this in the constructor, alas, because 
    // we need to refer to PayloadFactories our subclass will contain
    val PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]]; 
  }

  object P2P4 extends Subprotocol.Base( StringASCII_Exact3("p2p"), Unsigned16(4) ) {
    final object Hello extends Payload.Factory.Base[Hello]( this ){
      final case class Capabilities( elements : immutable.Set[(StringASCII_Exact3, Unsigned16)] )
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
      def subprotocol                   : Subprotocol = P2P4

      def rlp : RLPSerializing[Nothing] = throw new RuntimeException("NoFactory can't serialize/deserialize anything.")

      override lazy val offset : Unsigned16 = throw new RuntimeException("P2P4.NoFactory represents no valid offset.")
    }

    //NOTE: This sequence defines the offsets within the subprotocol!
    lazy val PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]] = {
      val defined = immutable.IndexedSeq( P2P4.Hello, P2P4.Disconnect, P2P4.Ping, P2P4.Pong ).lift;

      def factoryForIndex( i : Int ) : Payload.Factory[_] = defined( i ).getOrElse( P2P4.NoFactory )

      (0x00 until 0x10).map( factoryForIndex )
    }
  }

  final object Eth60 extends Subprotocol.Base( StringASCII_Exact3("eth"), Unsigned16(60) ) {
    final object Status extends Payload.Factory.Base[Status]( this );
    final case class Status (
      typeCode        : Unsigned16, 
      protocolVersion : Unsigned16,
      networkId       : Unsigned1,
      totalDifficulty : Unsigned256,
      bestHash        : EthHash,
      genesisHash     : EthHash
    ) extends Payload.Base[Status]( Status )

    final object NewBlockHashes extends Payload.Factory.Base[NewBlockHashes]( this );
    final case class NewBlockHashes ( 
      typeCode        : Unsigned16,
      hashes          : immutable.IndexedSeq[EthHash]
    ) extends Payload.Base[NewBlockHashes]( NewBlockHashes )

    final object Transactions extends Payload.Factory.Base[Transactions]( this );
    final case class Transactions (
      typeCode        : Unsigned16,
      transactions    : immutable.IndexedSeq[EthTransaction]
    ) extends Payload.Base[Transactions]( Transactions )

    final object GetBlockHashes extends Payload.Factory.Base[GetBlockHashes]( this );
    final case class GetBlockHashes (
      typeCode        : Unsigned16,
      hash            : EthHash,
      maxBlocks       : Unsigned256
    ) extends Payload.Base[GetBlockHashes]( GetBlockHashes )

    final object BlockHashes extends Payload.Factory.Base[BlockHashes]( this );
    final case class BlockHashes (
      typeCode        : Unsigned16,
      hashes          : immutable.IndexedSeq[EthHash]
    ) extends Payload.Base[BlockHashes]( BlockHashes )

    final object GetBlocks extends Payload.Factory.Base[GetBlocks]( this );
    final case class GetBlocks (
      typeCode        : Unsigned16,
      hashes          : immutable.IndexedSeq[EthHash]
    ) extends Payload.Base[GetBlocks]( GetBlocks )

    final object Blocks extends Payload.Factory.Base[Blocks]( this );
    final case class Blocks (
      typeCode        : Unsigned16,
      blocks          : immutable.IndexedSeq[EthBlock]
    ) extends Payload.Base[Blocks]( Blocks )

    final object NewBlock extends Payload.Factory.Base[NewBlock]( this );
    final case class NewBlock (
      typeCode        : Unsigned16,
      block           : EthBlock,
      totalDifficulty : Unsigned256
    ) extends Payload.Base[NewBlock]( NewBlock )

    lazy val PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]] =  immutable.IndexedSeq(
      Status,           // 0x00
      NewBlockHashes,   // 0x01
      Transactions,     // 0x02
      GetBlockHashes,   // 0x03
      BlockHashes,      // 0x04
      GetBlocks,        // 0x05
      Blocks,           // 0x06
      NewBlock          // 0x07
    )
  }
}
trait Subprotocol {
  def Name : StringASCII_Exact3;
  def Version : Unsigned16;
  def PayloadFactories : immutable.IndexedSeq[Payload.Factory[_]];

  lazy val Identifier : ( StringASCII_Exact3, Unsigned16 ) = ( Name, Version )
  def WideIdentifier : ( String, Int ) = ( Name.widen, Version.widen )
}
