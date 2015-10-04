package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable;
import com.mchange.sc.v1.consuela.ethereum.encoding.RLP;
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;
import com.mchange.lang.IntegerUtils;

object Packet {
  val SyncToken              = 0x22400891
  private val SyncTokenArray = IntegerUtils.byteArrayFromInt( SyncToken );
  val SyncTokenBytes         = ImmutableArraySeq.Byte( SyncTokenArray )

  val SyncTokenLen = 4
  val SizeLen      = 4;

  private val SizeStart    = SyncTokenLen;
  private val PayloadStart = SizeStart + SizeLen; 

  def encodeAsArray( payload : Seq[Byte] ) : Array[Byte] = {
    val SizeBytes = IntegerUtils.byteArrayFromInt( payload.length );

    val out = Array.ofDim[Byte]( SyncTokenLen + SizeLen + payload.length )
    System.arraycopy( SyncTokenArray, 0, out, 0, SyncTokenLen );
    System.arraycopy( SizeBytes, 0, out, SizeStart, SizeLen );
    payload.copyToArray( out, PayloadStart, payload.length );
    out
  }
  def encodeAsArray( payload : Array[Byte] ) : Array[Byte] = encodeAsArray( ImmutableArraySeq.Byte( payload ) )
  def encodeAsArray[T <: Payload[T]  : PayloadSerializing]( payload : T ) : Array[Byte] = encodeAsArray( RLP.encode( payload ) )

  def encode( payload : Seq[Byte] )   : immutable.Seq[Byte] = ImmutableArraySeq.Byte( encodeAsArray( payload ) )
  def encode( payload : Array[Byte] ) : immutable.Seq[Byte] = encode( ImmutableArraySeq.Byte( payload ) )
  def encode[T <: Payload[T] : PayloadSerializing]( payload : T ) : immutable.Seq[Byte] = this.encode( RLP.encode( payload ) )
}

