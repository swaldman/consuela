package com.mchange.sc.v1.consuela.bloom;

import scala.collection._;

import com.mchange.lang.LongUtils.byteArrayFromLong;

object BitSetBloom {
  def apply[T]( elements : T* )( implicit defn : Bloom.Definition[T] ) = new BitSetBloom( elements : _*)( defn );

  def empty[T : Bloom.Definition] = new BitSetBloom[T] {
    override def including( t : T )               : BitSetBloom[T] = new BitSetBloom[T]( t );
    override def includingAll( ts : Iterable[T] ) : BitSetBloom[T] = new BitSetBloom[T]( ts.toSeq : _* );

    override def mayContain( t :  T )              : Boolean = false;
    override def mayContainAll( ts : Iterable[T] ) : Boolean = ts.isEmpty;

    override def toUnsignedBigInt : BigInt = BigInt( 0 );

    override def +( other : BitSetBloom[T] ) : BitSetBloom[T] = other;
  }

  private def copyMutable( imm : immutable.BitSet ) : mutable.BitSet  = mutable.BitSet.fromBitMaskNoCopy( imm.toBitMask /* already a copy here */ );

  private def initializeBitSet[T]( contents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : immutable.BitSet = {
    updateMutableBitSet( mutable.BitSet.empty, contents )( definition ).toImmutable;
  }

  private def updateMutableBitSet[T]( oldElems : mutable.BitSet, newContents : Seq[T] )( implicit definition : Bloom.Definition[T] ) : mutable.BitSet = {
    newContents.foreach( elem => definition.indices( elem ).foreach( index => oldElems += index ) );
    oldElems
  }
}
class BitSetBloom[T : Bloom.Definition] private ( private val bitSet : immutable.BitSet ) extends Bloom[T, BitSetBloom[T]] {
  import BitSetBloom.{copyMutable, initializeBitSet, updateMutableBitSet};

  private val definition = implicitly[Bloom.Definition[T]];

  private def this( contents : T* ) = this( BitSetBloom.initializeBitSet( contents ) );

  def including( t : T ) : BitSetBloom[T] = new BitSetBloom[T]( bitSet ++ definition.checkedIndices( t ) );

  override def includingAll( ts : Iterable[T] ) : BitSetBloom[T] = {
    val mutable = copyMutable( bitSet );
    mutable ++= ts.flatMap( t => definition.checkedIndices( t ) );
    new BitSetBloom[T]( mutable.toImmutable )
  }

  def mayContain( t :  T ) : Boolean = definition.checkedIndices( t ).forall( bitSet.contains );

  override def mayContainAll( ts : Iterable[T] ) : Boolean = ts.toSet[T].flatMap( definition.checkedIndices ).forall( bitSet.contains );

  def toUnsignedBigInt : BigInt = BigInt( 1, bitSet.toBitMask.reverse.flatMap( byteArrayFromLong ) );

  def +( other : BitSetBloom[T] ) : BitSetBloom[T] = new BitSetBloom[T]( this.bitSet | other.bitSet );
}


