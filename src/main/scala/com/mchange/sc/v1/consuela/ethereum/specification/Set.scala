package com.mchange.sc.v1.consuela.ethereum.specification;

import java.math.BigInteger

object Set {
  object Unsigned extends Integral {
    override def contains( i : BigInt)      : Boolean = i >= 0;
    override def contains( i : Int )        : Boolean = i >= 0;
    override def contains( i : Long )       : Boolean = i >= 0L;
    override def contains( i : BigInteger ) : Boolean = i.compareTo( BigInteger.ZERO ) >= 0;
    override def mathRep : String = s"[0,\u221E)"
  }

  object Unsigned256 extends Integral.UnsignedWithBitLength( 256 );

  object Unsigned2048 extends Integral.UnsignedWithBitLength( 2048 );

  object ByteSeq1024 extends ByteSeq.LimitedLength( 1024 );

  object SignatureR extends Integral.ZeroUntil {
    val MaxValueExclusive : BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);
  }

  object SignatureS extends Integral.ZeroUntil {
    val MaxValueExclusive : BigInt = {
      val TWO = BigInt(2);
      (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977)
    }
  }

  object SignatureV extends Integral.MinUntil {
    val MinValueInclusive : BigInt = BigInt(27);
    val MaxValueExclusive : BigInt = BigInt(29);
    override def contains( i : Int )  : Boolean = i >= 27 && i < 29;
    override def contains( i : Long ) : Boolean = i >= 27L && i < 29L;
  }

  object Integral {
    trait MinUntil extends Integral {
      val MinValueInclusive : BigInt;
      val MaxValueExclusive : BigInt;
      override def contains( i : BigInt ) : Boolean = i >= MinValueInclusive && i < MaxValueExclusive;
      override def contains( i : BigInteger ) : Boolean = MinValueInclusive.bigInteger.compareTo(i) <= 0 && MaxValueExclusive.bigInteger.compareTo(i) > 0;
      override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
    }
    trait ZeroUntil extends MinUntil {
      override val MinValueInclusive = BigInt(0);
    }
    abstract class UnsignedWithBitLength( bitLength : Int ) extends ZeroUntil {
      val MaxValueExclusive : BigInt = BigInt(2).pow( bitLength );
    }
    abstract class UnsignedWithByteLength( byteLength : Int ) extends UnsignedWithBitLength( byteLength * 8 );
  }
  object ByteSeq {
    abstract class LimitedLength( val MaxLengthInclusive : Int ) extends Generic[Seq[Byte]] {
      def contains( seq : Seq[Byte] ) : Boolean = seq.length <= MaxLengthInclusive;

      override def mathRep : String = s"{ b | b \u2208 Seq[Byte] \u2227 b.length \u2264 ${MaxLengthInclusive} }"
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

