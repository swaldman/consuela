package com.mchange.sc.v1.consuela.ethereum.specification;

import com.mchange.sc.v1.consuela._;
import com.mchange.sc.v2.restrict._;

import java.math.BigInteger

import scala.collection._;

object Types {
  object Min { // inclusive minima
    val SignatureV : Byte = 27;
  }
  object Limit { // exclusive maxima
    val SignatureR : BigInt = BigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337", 10);
    val SignatureS : BigInt = { val TWO = BigInt(2); (TWO.pow(256)) - (TWO.pow(32)) - BigInt(977) }
    val SignatureV : Byte   = 29;
  }

  /*
   * 
   * Companion objects representing restricted types
   * 
   */ 
  object Unsigned      extends RestrictedBigInt.Unsigned[Unsigned]                          { override def create( value : BigInt ) = new Unsigned( value ); }
  object Unsigned8     extends RestrictedShort.UnsignedWithBitLength[Unsigned8]( 8 )        { override def create( value : Short )  = new Unsigned8( value ); }
  object Unsigned256   extends RestrictedBigInt.UnsignedWithBitLength[Unsigned256]( 256 )   { override def create( value : BigInt ) = new Unsigned256( value ); }
  object Unsigned2048  extends RestrictedBigInt.UnsignedWithBitLength[Unsigned2048]( 2048 ) { override def create( value : BigInt ) = new Unsigned2048( value ); }

  object SignatureR    extends RestrictedBigInt.ZeroUntil[SignatureR]( Limit.SignatureR )              { override def create( value : BigInt ) = new SignatureR( value ); }
  object SignatureS    extends RestrictedBigInt.ZeroUntil[SignatureS]( Limit.SignatureS )              { override def create( value : BigInt ) = new SignatureS( value ); }
  object SignatureV    extends RestrictedByte.MinUntil[SignatureV]( Min.SignatureV, Limit.SignatureV ) { override def create( value : Byte   ) = new SignatureV( value ); }

  object ByteSeqExact4  extends RestrictedByteSeq.ExactLength[ByteSeqExact4]( 4 )       { override def create( value : immutable.Seq[Byte] ) = new ByteSeqExact4( value ); }
  object ByteSeqExact8  extends RestrictedByteSeq.ExactLength[ByteSeqExact8]( 8 )       { override def create( value : immutable.Seq[Byte] ) = new ByteSeqExact8( value ); }
  object ByteSeqMax1024 extends RestrictedByteSeq.LimitedLength[ByteSeqMax1024]( 1024 ) { override def create( value : immutable.Seq[Byte] ) = new ByteSeqMax1024( value ); }

  /*
   * 
   * Value classes implementing types
   * 
   */ 
  class Unsigned     private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  class Unsigned8    private ( val value : Short  ) extends AnyVal with RestrictedType.Element[Short];
  class Unsigned256  private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  class Unsigned2048 private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  class SignatureR   private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  class SignatureS   private ( val value : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];
  class SignatureV   private ( val value : Byte   ) extends AnyVal with RestrictedType.Element[Byte];

  class ByteSeqExact4  private ( val value : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  class ByteSeqExact8  private ( val value : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
  class ByteSeqMax1024 private ( val value : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]];
}
