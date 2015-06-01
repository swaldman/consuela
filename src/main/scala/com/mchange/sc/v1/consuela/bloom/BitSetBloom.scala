/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

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

  override def equals( other : Any ) : Boolean = {
    other match {
      case bsb : BitSetBloom[_] => this.bitSet == bsb.bitSet && this.definition == bsb.definition; //definition equality ensures same element type T
      case _                    => false;
    }
  }

  override def hashCode : Int = bitSet.hashCode ^ definition.hashCode;
}


