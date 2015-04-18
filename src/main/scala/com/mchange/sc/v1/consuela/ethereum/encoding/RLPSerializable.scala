package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela.Implicits._;
import scala.collection._;

object RLPSerializable {
  trait Companion[T <: RLPSerializable[T]] {

    // implement the following two methods in the companion object, let the class
    // extend RLPSerializable or RLPSerializable.LavyVal and you are done.
    def toRLPEncodable( rlpSerializable : T )               : RLP.Encodable;
    def fromRLPEncodable( encodable : RLP.Encodable.Basic ) : T;

    def decodeRLP( bytes : Seq[Byte] ) : ( T, Seq[Byte] ) = {
      val (encodable, rest) = RLP.decode( bytes );
      ( fromRLPEncodable( encodable ), rest )
    }
    def decodeCompleteRLP( bytes : Seq[Byte] ) : T = {
      val ( serializable, rest ) = decodeRLP( bytes );
      if ( rest.length > 0 ) {
        throw new IllegalArgumentException(
          s"Companion object for ${SimpleName} decodeCompleteRLP(...) expects exactly the bytes of a ${SimpleName}; received bytes for ${serializable} with 0x${rest.hex} left over."
        )
      } else {
        serializable
      }
    }
    def encodeRLP( rlpSerializable : T ) : immutable.Seq[Byte] = RLP.encode( toRLPEncodable( rlpSerializable ) );

    private lazy val SimpleName = this.getClass.getSimpleName.reverse.dropWhile( _ == '$' ).map( c => if ( c == '$' ) '.' else c ).reverse;
  }
  trait LazyVal[T <: LazyVal[T]] extends RLPSerializable[T] {
    protected val companion : RLPSerializable.Companion[T];

    override lazy val rlpEncodable : RLP.Encodable       = super.rlpEncodable;
    override lazy val rlpBytes     : immutable.Seq[Byte] = super.rlpBytes;
  }
}
trait RLPSerializable[T <: RLPSerializable[T]] {
  protected def companion : RLPSerializable.Companion[T];

  def rlpEncodable : RLP.Encodable       = companion.toRLPEncodable( this.asInstanceOf[T] );
  def rlpBytes     : immutable.Seq[Byte] = RLP.encode( this.rlpEncodable );
}

