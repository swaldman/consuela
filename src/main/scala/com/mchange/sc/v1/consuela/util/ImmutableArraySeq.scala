package com.mchange.sc.v1.consuela.ethereum.util;

import scala.collection._;

import java.util.Arrays;

object ImmutableArraySeq {
  abstract class Abstract[A] protected ( private val inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends immutable.IndexedSeq[A] {
    override def apply( i : Int ) : A = inner.apply(i);

    override def iterator : Iterator[A] = inner.iterator;

    override def length : Int = inner.length;

    override def toArray[B >: A](implicit btag : scala.reflect.ClassTag[B]) : Array[B] = {
      if (atag.runtimeClass == btag.runtimeClass) {
        inner.clone().asInstanceOf[Array[B]]
      } else {
        val src = inner
        val len = src.length;
        val out = btag.newArray( len );
        System.arraycopy( src, 0, out, 0, len );
        out
      }
    }
  }
  final class Byte private ( private val byteInner : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) extends Abstract[scala.Byte]( byteInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Byte => Arrays.equals( this.byteInner, other.byteInner );
        case whatever                       => super.equals( this, whatever );
      }
    }
  }
}
final class ImmutableArraySeq[A] private ( inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends ImmutableArraySeq.Abstract[A]( inner );



