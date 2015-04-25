package com.mchange.sc.v1.consuela.bloom;

import scala.collection._;

import com.mchange.sc.v1.consuela._;

import scala.math.{log,exp,pow};

/**
 *  I'm indebted to Bill Mill for his excellent tutorial:
 * 
 *     http://billmill.org/bloomfilter-tutorial/
 */ 
object Bloom {
  private val LN2 : Double = log(2);

  def optimalNumHashes( ifBitLength : Int, ifNumEntries : Int ) : Double = {
    val m : Double = ifBitLength;
    val n : Double = ifNumEntries;
    (m/n) * LN2;
  }

  def approximateFalsePositiveRate( ifNumHashes : Int, ifBitLength : Int, ifNumEntries : Int ) : Double = {
    val k : Double = ifNumHashes;
    val m : Double = ifBitLength;
    val n : Double = ifNumEntries;
    pow( (1d - exp((-k * n) / m)), k )
  }

  trait Definition[T] {
    /**
     * Indices should be hashes % BitLength, ie modded down to ints in [0, BitLength)
     *
     * The MAXIMUM size of the set should be NumHashes, since multiple hashes may by
     * coincidence yield the same index.  
     */  
    def indices( t : T ) : immutable.Set[Int];

    val NumHashes : Int;
    val BitLength : Int;

    // better be lazy, or it will be zero!
    lazy val ByteLength : Int = BitLength / 8 + ( if ( BitLength % 8 == 0 ) 0 else 1 );

    def approximateFalsePositiveRate( ifNumEntries : Int ) : Double = Bloom.approximateFalsePositiveRate( this.NumHashes, this.BitLength, ifNumEntries );
  }
}
trait Bloom[T, I<:Bloom[T,I]] {
  self : I =>

  protected val definition : Bloom.Definition[T];

  def including( t : T )               : I;
  def includingAll( ts : Iterable[T] ) : I = ts.foldLeft( self )( ( bloom, t ) => bloom.including(t) );
  final def including( ts : T*)        : I = includingAll( ts );

  def mayContain( t :  T)               : Boolean;
  def mayContainAll( ts : Iterable[T] ) : Boolean = ts.toSet.foldLeft( true )( (ok, next) => ok && this.mayContain( next ) );

  /**
   *  bytes of a big-endian value, zero-indexed from the least-significant-bit.
   */
  def toByteArray : Array[Byte]
 
  /**
   *  bytes of a big-endian value, zero-indexed from the least-significant-bit.
   *
   *  consider overriding as lazy val 
   */
  def bytes : immutable.Seq[Byte]; 

  /**
   *  consider overriding as lazy val 
   */
  def toUnsignedBigInt : BigInt;

  def +( other : I ) : I;

  def bitsSet : Int;

  def bitLength  : Int = definition.BitLength;
  def byteLength : Int = definition.ByteLength;

  def approximateFalsePositiveRate( ifNumEntries : Int ) : Double = definition.approximateFalsePositiveRate( ifNumEntries );
}

