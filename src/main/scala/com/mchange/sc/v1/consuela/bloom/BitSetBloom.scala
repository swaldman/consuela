package com.mchange.sc.v1.consuela.bloom;

import com.mchange.sc.v1.consuela._;

import scala.collection._;

import com.mchange.lang.LongUtils.byteArrayFromLong;

object BitSetBloom {
  def apply[T]( elements : T* )( implicit defn : Bloom.Definition[T] ) = new BitSetBloom( elements : _*)( defn );

  def empty[T : Bloom.Definition] = new BitSetBloom[T]( immutable.BitSet.empty )

  private def copyMutable( imm : immutable.BitSet ) : mutable.BitSet  = mutable.BitSet.fromBitMaskNoCopy( imm.toBitMask /* already a copy here */ );

  private def initializeBitSet[T]( contents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : immutable.BitSet = {
    updateMutableBitSet( mutable.BitSet.empty, contents )( definition ).toImmutable;
  }

  private def updateMutableBitSet[T]( oldElems : mutable.BitSet, newContents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : mutable.BitSet = {
    newContents.foreach( elem => definition.indices( elem ).foreach( index => oldElems += index ) );
    oldElems
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

  private lazy val _array : Array[Byte] = math.zeroPadLeft( bitSet.toBitMask.reverse.flatMap( byteArrayFromLong ), byteLength ); // mustn't ever be mutated

  def toByteArray : Array[Byte] = _array.clone();

  lazy val toUnsignedBigInt : BigInt = BigInt( 1, _array );

  override lazy val bytes : immutable.Seq[Byte] = util.ImmutableArraySeq.Byte.createNoCopy( _array );

  def +( other : BitSetBloom[T] ) : BitSetBloom[T] = new BitSetBloom[T]( this.bitSet | other.bitSet );

  override def toString : String = s"${this.getClass.getSimpleName}[${this.bytes.hex}]" 
}


