package com.mchange.sc.v1.consuela.bloom;

import scala.collection._;

import com.mchange.sc.v1.consuela._;

/**
 *  I'm indebted to Bill Mill for his excellent tutorial:
 * 
 *     http://billmill.org/bloomfilter-tutorial/
 */ 
object Bloom {
  trait Definition[T] {
    /**
     * Hashes % bitLength, ie modded down to ints in [0, bitLength)
     */  
    def indices( t : T ) : immutable.Set[Int];
    val bitLength : Int;

    // better be lazy, or it will be zero!
    lazy val byteLength : Int = bitLength / 8 + ( if ( bitLength % 8 == 0 ) 0 else 1 ); 
  }
}
trait Bloom[T, I<:Bloom[T,I]] {
  self : I =>

  protected val definition : Bloom.Definition[T];

  def including( t : T )  : I;
  def includingAll( ts : Iterable[T] ) : I = ts.foldLeft( self )( ( bloom, t ) => bloom.including(t) );
  final def including( ts : T*) : I = includingAll( ts );


  def mayContain( t :  T) : Boolean;
  def mayContainAll( ts : Iterable[T] ) : Boolean = ts.toSet.foldLeft( true )( (ok, next) => ok && this.mayContain( next ) );

  def toUnsignedBigInt : BigInt;

  def +( other : I ) : I;

  def bitLength  : Int = definition.bitLength;
  def byteLength : Int = definition.byteLength;

  /**
   *  bytes of a big-endian value, zero-indexed from the least-significant-bit.
   */ 
  lazy val bytes : immutable.Seq[Byte] = util.ImmutableArraySeq.Byte( this.toUnsignedBigInt.unsignedBytes( definition.byteLength ) );
}

