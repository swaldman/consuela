package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable;
import scala.util.Try;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v1.consuela.ethereum.encoding.RLP;
import com.mchange.sc.v1.consuela.ethereum.specification.Types.Unsigned16;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;
import com.mchange.lang.IntegerUtils;

import com.mchange.sc.v2.failable._

object Packet {
  val SyncToken              = 0x22400891
  private val SyncTokenArray = IntegerUtils.byteArrayFromInt( SyncToken )
  val SyncTokenBytes         = ImmutableArraySeq.Byte( SyncTokenArray )

  val SyncTokenLen = 4

  val SizeStart    = SyncTokenLen;
  val SizeLen      = 4

  val PayloadStart = SyncTokenLen + SizeLen


  private def encodeAsArray( payload : Seq[Byte] ) : Array[Byte] = {
    val SizeBytes = IntegerUtils.byteArrayFromInt( payload.length );

    val out = Array.ofDim[Byte]( SyncTokenLen + SizeLen + payload.length )
    System.arraycopy( SyncTokenArray, 0, out, 0, SyncTokenLen );
    System.arraycopy( SizeBytes, 0, out, SizeStart, SizeLen );
    payload.copyToArray( out, PayloadStart, payload.length );
    out
  }
  private def encodeAsArray( payload : Array[Byte] ) : Array[Byte] = encodeAsArray( ImmutableArraySeq.Byte( payload ) )
  def encodeAsArray[P <: Payload[P]]( payload : P )( implicit session : Session ) : Failable[Array[Byte]] = _encode( payload, encodeAsArray )( session )

  private def encode( payload : Seq[Byte] )   : immutable.Seq[Byte] = ImmutableArraySeq.Byte( encodeAsArray( payload ) )
  private def encode( payload : Array[Byte] ) : immutable.Seq[Byte] = encode( ImmutableArraySeq.Byte( payload ) )
  def encode[P <: Payload[P]]( payload : P )( implicit session : Session ) : Failable[immutable.Seq[Byte]] = _encode( payload, encode )( session )

  private def _encode[P <: Payload[P], R]( payload : P, f : Seq[Byte] => R )( session : Session ) : Failable[R] = {
    try {
      val pf : Payload.Factory[P] = session.payloadFactories( payload.typeCode.widen ).asInstanceOf[Payload.Factory[P]]
      val repayload = pf.validate( payload )
      repayload.map( pf.rlp.encode(_) ).map( f )
    } catch Poop
  }

  def decode( session : Session, packet : Seq[Byte] ) : Failable[Payload[_]] = {
    def badHeaderMessage : String                    = s"Four byte packet header 0x${ packet.slice(0, SyncTokenLen).hex } is not the expected value 0x${SyncTokenBytes.hex}"

    def headerCheck   : Failable[Unit] = if ((0 until SyncTokenLen).forall( i => packet(i) == SyncTokenBytes( i ) )) succeed( () ) else fail( badHeaderMessage );
    def payloadLength : Failable[Int]  = Try( IntegerUtils.intFromByteArray( packet.slice( SizeStart, PayloadStart ).toArray, 0 ) ).toFailable
    def payloadElement( plen : Int ) : Failable[RLP.Element.Seq] = {
      Try ( RLP.Element.decodeComplete( packet.slice( PayloadStart, PayloadStart + plen ) ).asInstanceOf[RLP.Element.Seq] ).toFailable
    }
    def peekTypeCode( rlpseq : RLP.Element.Seq ) : Failable[Unsigned16] = {
      Try {
        rlpseq match {
          case RLP.Element.Seq.of( RLP.Element.ByteSeq( typeCodeBytes ), _* ) => succeed( Unsigned16( typeCodeBytes.toUnsignedBigInt ) )
          case _                                                              => fail( s"Unexpected RLP structure for payload ${rlpseq}" )
        }
      }.toFailable.flatten
    }

    for {
      _        <- headerCheck
      plen     <- payloadLength
      elem     <- payloadElement( plen )
      typeCode <- peekTypeCode( elem )
      payload  <- session.payloadFactories( typeCode.widen ).rlp.fromElement( elem )
    } yield {
      payload.asInstanceOf[Payload[_]]
    }
  }
}

