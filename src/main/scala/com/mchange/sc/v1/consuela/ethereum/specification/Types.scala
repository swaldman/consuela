package com.mchange.sc.v1.consuela.ethereum.specification;

import com.mchange.sc.v1.consuela._;

import java.math.BigInteger

import scala.collection._;

/**
 * This file is an endless swamp of code duplication and poor abstraction.
 * I hate that more than you do. It's left this way because comparisons and
 * value class are susceptible to promotions and boxing if you work with them
 * conveniently, and we are trying to avoid that here.
 */ 
object Types {
  val BI_0 = BigInt(0);

  object Unsigned extends Integral {
    override def contains( i : BigInt)      : Boolean = i >= BI_0;
    override def contains( i : Int )        : Boolean = i >= 0;
    override def contains( i : Long )       : Boolean = i >= 0L;
    override def contains( i : BigInteger ) : Boolean = i.compareTo( BigInteger.ZERO ) >= 0;
    override def mathRep : String = s"[0,\u221E)"

    def apply( value : BigInt ) : Unsigned = {
      require( value elem_!: Unsigned );
      new Unsigned( value )
    }
  }
  class Unsigned private ( val value : BigInt ) extends AnyVal { override def toString : String = s"Unsigned(${value})" }

  object Unsigned8 extends Integral.Int.UnsignedWithBitLength( 8 ) {
    def apply( value : Int ) : Unsigned8 = {
      require( value elem_!: Unsigned8 );
      new Unsigned8( value )
    }
  }
  class Unsigned8 private ( val value : Int ) extends AnyVal { override def toString : String = s"Unsigned8(${value})" }

  object Unsigned31 extends Integral.Int.UnsignedWithBitLength( 31 ) {
    def apply( value : Int ) : Unsigned31 = {
      require( value elem_!: Unsigned31 );
      new Unsigned31( value )
    }
  }
  class Unsigned31 private ( val value : Int ) extends AnyVal { override def toString : String = s"Unsigned31(${value})" }

  object Unsigned32 extends Integral.Long.UnsignedWithBitLength( 32 ) {
    def apply( value : Long ) : Unsigned32 = {
      require( value elem_!: Unsigned32 );
      new Unsigned32( value )
    }
  }
  class Unsigned32 private ( val value : Long ) extends AnyVal { override def toString : String = s"Unsigned32(${value})" }

  object Unsigned256 extends Integral.BigInt.UnsignedWithBitLength( 256 ) {
    def apply( value : BigInt ) : Unsigned256 = {
      require( value elem_!: Unsigned256 );
      new Unsigned256( value )
    }
  }
  class Unsigned256 private ( val value : BigInt ) extends AnyVal { override def toString : String = s"Unsigned256(${value})" }

  object Unsigned2048 extends Integral.BigInt.UnsignedWithBitLength( 2048 ) {
    def apply( value : BigInt ) : Unsigned2048 = {
      require( value elem_!: Unsigned2048 );
      new Unsigned2048( value )
    }
  }
  class Unsigned2048 private ( val value : BigInt ) extends AnyVal { override def toString : String = s"Unsigned2048(${value})" }

  object SignatureR extends Integral.BigInt.ZeroUntil {
    val MaxValueExclusive : BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);

    def apply( value : BigInt ) : SignatureR = {
      require( value elem_!: SignatureR );
      new SignatureR( value )
    }
  }
  class SignatureR private ( val value : BigInt ) extends AnyVal { override def toString : String = s"SignatureR(${value})" }

  object SignatureS extends Integral.BigInt.ZeroUntil {
    val MaxValueExclusive : BigInt = {
      val TWO = BigInt(2);
      (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977)
    }

    def apply( value : BigInt ) : SignatureS = {
      require( value elem_!: SignatureS );
      new SignatureS( value )
    }
  }
  class SignatureS private ( val value : BigInt ) extends AnyVal { override def toString : String = s"SignatureS(${value})" }

  object SignatureV extends Integral.Int.MinUntil {
    val MinValueInclusive : Int = 27;
    val MaxValueExclusive : Int = 29;

    def apply( value : Int ) : SignatureV = {
      require( value elem_!: SignatureV );
      new SignatureV( value )
    }
  }
  class SignatureV private ( val value : Int ) extends AnyVal { override def toString : String = s"SignatureV(${value})" }

  object ByteSeqExact4 extends ByteSeq.ExactLength( 4 ) {
    def apply( value : immutable.Seq[Byte] ) : ByteSeqExact4 = {
      require( value elem_!: ByteSeqExact4 );
      new ByteSeqExact4( value )
    }
  }
  class ByteSeqExact4 private ( val value : immutable.Seq[Byte] ) extends AnyVal { override def toString : String = s"ByteSeqExact4(0x${value.hex})" }

  object ByteSeqExact8  extends ByteSeq.ExactLength( 8 ) {
    def apply( value : immutable.Seq[Byte] ) : ByteSeqExact8 = {
      require( value elem_!: ByteSeqExact8 );
      new ByteSeqExact8( value )
    }
  }
  class ByteSeqExact8 private ( val value : immutable.Seq[Byte] ) extends AnyVal { override def toString : String = s"ByteSeqExact8(0x${value.hex})" }

  object ByteSeqMax1024 extends ByteSeq.LimitedLength( 1024 ) {
    def apply( value : immutable.Seq[Byte] ) : ByteSeqMax1024 = {
      require( value elem_!: ByteSeqMax1024 );
      new ByteSeqMax1024( value )
    }
  }
  class ByteSeqMax1024 private ( val value : immutable.Seq[Byte] ) extends AnyVal { override def toString : String = s"ByteSeqMax1024(0x${value.hex})" }

  object ByteSeq {
    abstract class LimitedLength( val MaxLengthInclusive : Int ) extends Generic[immutable.Seq[Byte]] {
      def contains( seq : immutable.Seq[Byte] ) : Boolean = seq.length <= MaxLengthInclusive;

      override def mathRep : String = s"{ b | b \u2208 immutable.Seq[Byte] \u2227 b.length \u2264 ${MaxLengthInclusive} }"
    }

    abstract class ExactLength( val RequiredLength : Int ) extends Generic[immutable.Seq[Byte]] {
      def contains( seq : immutable.Seq[Byte] ) : Boolean = seq.length == RequiredLength;

      override def mathRep : String = s"{ b | b \u2208 immutable.Seq[Byte] \u2227 b.length = ${RequiredLength} }"
    }
  }

  object Integral {

    // I hate all the code duplication here, but it's not obvious to me how to abstract it away.
    // We have three levels of Integral type to avoid promotions and implicit conversions on likely comparisons
    // Obvious techniques for eliminating the duplicate code would force unnecessary promotions / conversions.

    object Int {
      trait MinUntil extends Integral {
        val MinValueInclusive : Int;
        val MaxValueExclusive : Int;
        override def contains( i : Int ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : Long ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : scala.math.BigInt ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : BigInteger ) : Boolean = contains( scala.math.BigInt( i ) );
        override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
      }
      trait ZeroUntil extends MinUntil {
        override val MinValueInclusive = 0;
      }
      abstract class UnsignedWithBitLength( bitLength : Int ) extends ZeroUntil {
        require( 
          bitLength < 32, 
          s"Use Integral.Long.UnsignedWithBitLength or Integral.BigInt.UnsignedWithBitLength for bit length of 32 or more. ( bitLength -> ${bitLength} )" 
        );

        val MaxValueExclusive : Int = 1 << bitLength;
      }
      abstract class UnsignedWithByteLength( byteLength : Int ) extends UnsignedWithBitLength( byteLength * 8 );
    }
    object Long {
      trait MinUntil extends Integral {
        val MinValueInclusive : Long;
        val MaxValueExclusive : Long;
        override def contains( i : Int ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : Long ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : scala.math.BigInt ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : BigInteger ) : Boolean = contains( scala.math.BigInt( i ) );
        override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
      }
      trait ZeroUntil extends MinUntil {
        override val MinValueInclusive = 0L;
      }
      abstract class UnsignedWithBitLength( bitLength : Int ) extends ZeroUntil {
        require( 
          bitLength >= 32, 
          s"Use Integral.Int.UnsignedWithBitLength for bit length of 31 or less. ( bitLength -> ${bitLength} )" 
        );
        require( 
          bitLength < 64, 
          s"Use Integral.BigInt.UnsignedWithBitLength for bit length of 64 or more. ( bitLength -> ${bitLength} )" 
        );

        val MaxValueExclusive : Long = 1 << bitLength;
      }
      abstract class UnsignedWithByteLength( byteLength : Int ) extends UnsignedWithBitLength( byteLength * 8 );
    }
    object BigInt {
      trait MinUntil extends Integral {
        val MinValueInclusive : scala.math.BigInt;
        val MaxValueExclusive : scala.math.BigInt;
        override def contains( i : scala.math.BigInt ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
        override def contains( i : BigInteger ) : Boolean = MinValueInclusive.bigInteger.compareTo(i) <= 0 && MaxValueExclusive.bigInteger.compareTo(i) > 0;
        override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
      }
      trait ZeroUntil extends MinUntil {
        override val MinValueInclusive = scala.math.BigInt(0);
      }
      abstract class UnsignedWithBitLength( bitLength : Int ) extends ZeroUntil {
        require( bitLength >= 64, s"Use Integral.Int.UnsignedWithBitLength or Integral.Long.UnsignedWithBitLength for bit length of 63 or less. ( bitLength -> ${bitLength} )" );

        val MaxValueExclusive : scala.math.BigInt = scala.math.BigInt(2).pow( bitLength );
      }
      abstract class UnsignedWithByteLength( byteLength : Int ) extends UnsignedWithBitLength( byteLength * 8 );
    }
  }

  // it feels like I should be making use of generics somehow to avoid the code repetion
  // but i'm not sure just how
  trait Integral extends Set {
    // always override this
    def contains( i : BigInt) : Boolean;

    // maybe override these for performance, if you'll do tests on smaller integrals
    def contains( i : Int )        : Boolean = this.contains( BigInt( i ) );
    def contains( i : Long )       : Boolean = this.contains( BigInt( i ) );
    def contains( i : BigInteger ) : Boolean = this.contains( BigInt( i ) );

    final def elem_: ( i : BigInteger) : Boolean = this.contains( i );
    final def elem_: ( i : Int)        : Boolean = this.contains( i );
    final def elem_: ( i : Long)       : Boolean = this.contains( i );
    final def elem_: ( i : BigInt)     : Boolean = this.contains( i );

    final def elem_!: ( i : BigInteger) : Boolean = this.contains( i ) || ( throw new IllegalArgumentException( badValueMessage(i) ) );
    final def elem_!: ( i : Int)        : Boolean = this.contains( i ) || ( throw new IllegalArgumentException( badValueMessage(i) ) );
    final def elem_!: ( i : Long)       : Boolean = this.contains( i ) || ( throw new IllegalArgumentException( badValueMessage(i) ) );
    final def elem_!: ( i : BigInt)     : Boolean = this.contains( i ) || ( throw new IllegalArgumentException( badValueMessage(i) ) );
  }
  trait Generic[T] extends Set {
    def contains( t : T ) : Boolean;

    final def elem_:  ( t : T )        : Boolean = this.contains( t );
    final def elem_!: ( t : T )        : Boolean = this.contains( t ) || ( throw new IllegalArgumentException( badValueMessage(t) ) );
  }
}
trait Set {
  def mathRep : String;
  override def toString : String = this.getClass.getName + mathRep;
  def notMemberMessage( a : Any ) = s"${a} \u2209 ${mathRep}";
  def badValueMessage( a : Any ) = "Bad value: " + notMemberMessage( a )
}

