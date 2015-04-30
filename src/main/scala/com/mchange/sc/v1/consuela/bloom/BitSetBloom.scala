package com.mchange.sc.v1.consuela.bloom;

import com.mchange.sc.v1.consuela._;

import scala.collection._;

import com.mchange.lang.LongUtils.{byteArrayFromLong, longFromByteArray};
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq;

object BitSetBloom {
  def apply[T]( elements : T* )( implicit defn : Bloom.Definition[T] ) = new BitSetBloom( elements : _*)( defn );

  def empty[T : Bloom.Definition] = new BitSetBloom[T]( immutable.BitSet.empty )

  // icky code duplication, i should probably choose one of these an implement the other in terms of the first
  def fromBytes[T]( bytes : Array[Byte] )( implicit defn : Bloom.Definition[T] ) : BitSetBloom[T] = {
    require( bytes.length == defn.ByteLength, s"You can't create a ${defn.ByteLength} byte BitSetBloom from a ${bytes.length} byte sequences; the lengths must be equal." );
    new BitSetBloom[T]( immutable.BitSet.fromBitMaskNoCopy( bitmaskFromBytes( bytes ) ) )
  }
  def fromBytes[T]( bytes : Seq[Byte] )( implicit defn : Bloom.Definition[T] ) : BitSetBloom[T] = {
    require( bytes.length == defn.ByteLength, s"You can't create a ${defn.ByteLength} byte BitSetBloom from a ${bytes.length} byte sequences; the lengths must be equal." );
    new BitSetBloom[T]( immutable.BitSet.fromBitMaskNoCopy( bitmaskFromBytes( bytes ) ) )
  }

  private def copyMutable( imm : immutable.BitSet ) : mutable.BitSet  = mutable.BitSet.fromBitMaskNoCopy( imm.toBitMask /* already a copy here */ );

  private def initializeBitSet[T]( contents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : immutable.BitSet = {
    updateMutableBitSet( mutable.BitSet.empty, contents )( definition ).toImmutable;
  }

  private def updateMutableBitSet[T]( oldElems : mutable.BitSet, newContents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : mutable.BitSet = {
    newContents.foreach( elem => definition.indices( elem ).foreach( index => oldElems += index ) );
    oldElems
  }

  // icky code duplication, i should probably choose one of these an implement the other in terms of the first
  private def bitmaskFromBytes( bytes : Array[Byte] ) : Array[Long] = {
    val remainder     = bytes.length % 8;
    val paddedToLongs = if ( remainder == 0 ) bytes else (Array.ofDim[Byte]( 8 - remainder ) ++ bytes);
    paddedToLongs.grouped(8).map( longFromByteArray( _, 0 ) ).toList.reverse.toArray
  }
  private def bitmaskFromBytes( bytes : Seq[Byte] ) : Array[Long] = {
    val remainder     = bytes.length % 8;
    val paddedToLongs = if ( remainder == 0 ) bytes else (immutable.Seq.fill[Byte]( 8 - remainder )(0) ++ bytes);
    paddedToLongs.grouped(8).map( _.toArray ).map( longFromByteArray( _, 0 ) ).toList.reverse.toArray
  }
}
final class BitSetBloom[T : Bloom.Definition] private ( private val bitSet : immutable.BitSet ) extends Bloom[T, BitSetBloom[T]] {
  import BitSetBloom.{copyMutable, initializeBitSet, updateMutableBitSet};

  protected val definition = implicitly[Bloom.Definition[T]];

  private def this( contents : T* ) = this( BitSetBloom.initializeBitSet( contents ) );

  def including( t : T ) : BitSetBloom[T] = new BitSetBloom[T]( bitSet ++ definition.indices( t ) );

  override def includingAll( ts : Iterable[T] ) : BitSetBloom[T] = {
    val mutable = copyMutable( bitSet );
    mutable ++= ts.flatMap( t => definition.indices( t ) );
    new BitSetBloom[T]( mutable.toImmutable )
  }

  def mayContain( t :  T ) : Boolean = definition.indices( t ).forall( bitSet.contains );

  override def mayContainAll( ts : Iterable[T] ) : Boolean = ts.toSet[T].flatMap( definition.indices ).forall( bitSet.contains );

  def bitsSet : Int = bitSet.size;

  private lazy val _array : Array[Byte] = { // mustn't ever be mutated
    val rawBytes = bitSet.toBitMask.reverse.flatMap( byteArrayFromLong );
    val smallBytes = if ( rawBytes.length > byteLength ) rawBytes.dropWhile( _ == 0 ) else rawBytes;
    math.zeroPadLeft( smallBytes, byteLength ); 
  }

  def toByteArray : Array[Byte] = _array.clone();

  lazy val toUnsignedBigInt : BigInt = BigInt( 1, _array );

  override lazy val bytes : immutable.Seq[Byte] = ImmutableArraySeq.Byte.createNoCopy( _array );

  def +( other : BitSetBloom[T] ) : BitSetBloom[T] = new BitSetBloom[T]( this.bitSet | other.bitSet );

  override def toString : String = s"${this.getClass.getSimpleName}[${this.bytes.hex}]" 
}


