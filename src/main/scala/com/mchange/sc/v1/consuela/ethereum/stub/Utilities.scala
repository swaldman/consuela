package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.EthLogEntry
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Client.Log.Filter.TopicRestriction
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent

final object Utilities {
  val Zero = sol.UInt256(0)

  def anyIntegralToBigInt( a : Any ) : BigInt = {
    a match {
      case b  : Byte   => BigInt( b )
      case s  : Short  => BigInt( s )
      case i  : Int    => BigInt( i )
      case l  : Long   => BigInt( l )
      case bi : BigInt => bi
      case _           => throw new StubException( s"${a} is not an integral type, cannot be converted to BigInt." )
    }
  }

  def topicRestriction( topicSeq : Seq[EthLogEntry.Topic] ) : TopicRestriction = {
    if ( topicSeq.isEmpty ) TopicRestriction.Any else TopicRestriction.AnyOf( topicSeq : _* )
  }

  def assertNamedEvent( event : SolidityEvent, metadata : Event.Metadata ) : SolidityEvent.Named = {
    event match {
      case named : SolidityEvent.Named => named
      case anonymous : SolidityEvent.Anonymous => throw new UnexpectedEventException( anonymous, metadata, s"Anonymous event found where named event expected: ${anonymous}" )
    }
  }
}
