package com.mchange.sc.v1.consuela.ethereum.encoding;

import com.mchange.sc.v1.consuela._;
import scala.collection._;

object RLPSerializable {
  trait Companion[T <: RLPSerializable[T]] extends RLPSerializing.AbstractWrapper[T];

  trait LazyVal[T <: LazyVal[T]] extends RLPSerializable[T] {
    protected val companion : RLPSerializable.Companion[T];

    override lazy val rlpElement : RLP.Element       = super.rlpElement;
    override lazy val rlpBytes     : immutable.Seq[Byte] = super.rlpBytes;
  }
  //private lazy val SimpleName = this.getClass.getSimpleName.reverse.dropWhile( _ == '$' ).map( c => if ( c == '$' ) '.' else c ).reverse;
}
trait RLPSerializable[T <: RLPSerializable[T]] {
  protected def companion : RLPSerializable.Companion[T];

  def rlpElement : RLP.Element       = companion.toRLPElement( this.asInstanceOf[T] );
  def rlpBytes     : immutable.Seq[Byte] = RLP.Element.encode( this.rlpElement );
}

