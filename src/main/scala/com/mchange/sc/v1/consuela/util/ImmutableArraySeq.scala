package com.mchange.sc.v1.consuela.util;

import scala.collection._;

import java.util.Arrays;

object ImmutableArraySeq {
  def apply[A]( source : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) = new ImmutableArraySeq[A]( source.clone() )( atag );

  /**
   * Be sure you know WTF you are doing, that nothing else mutates this array.
   */  
  def createNoCopy[A]( source : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) = new ImmutableArraySeq( source )( atag );

  abstract class Abstract[A] protected ( private val inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends immutable.IndexedSeq[A] {
    override def apply( i : scala.Int ) : A = inner.apply(i);

    override def iterator : Iterator[A] = inner.iterator;

    override def length : scala.Int = inner.length;

    override def toArray[B >: A](implicit btag : scala.reflect.ClassTag[B]) : Array[B] = {
      try {
        if (atag.runtimeClass == btag.runtimeClass) {
          inner.clone().asInstanceOf[Array[B]]
        } else {
          val src = inner
          val len = src.length;
          val out = btag.newArray( len );
          System.arraycopy( src, 0, out, 0, len );
          out
        }
      } catch {
        case e : ArrayStoreException => { // if we ask for a primitive array as an Any (rare)
          Array[B]( inner : _* )          // this formulation boxes
        }
      }
    }
  }

  object Byte {
    def apply( source : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) = new ImmutableArraySeq.Byte( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else mutates this array.
     */  
    def createNoCopy( source : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) = new ImmutableArraySeq.Byte( source )( atag );
  }
  final class Byte private ( private val byteInner : Array[scala.Byte] )( implicit atag : scala.reflect.ClassTag[scala.Byte] ) extends Abstract[scala.Byte]( byteInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Byte => Arrays.equals( this.byteInner, other.byteInner );
        case whatever                       => super.equals( whatever );
      }
    }
  }

  object Int {
    def apply( source : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) = new ImmutableArraySeq.Int( source.clone() )( atag );

    /**
     * Be sure you know WTF you are doing, that nothing else mutates this array.
     */  
    def createNoCopy( source : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) = new ImmutableArraySeq.Int( source )( atag );
  }
  final class Int private ( private val intInner : Array[scala.Int] )( implicit atag : scala.reflect.ClassTag[scala.Int] ) extends Abstract[scala.Int]( intInner ) {
    override def equals( o : Any ) : Boolean = {
      o match {
        case other : ImmutableArraySeq.Int => Arrays.equals( this.intInner, other.intInner );
        case whatever                      => super.equals( whatever );
      }
    }
  }
}
final class ImmutableArraySeq[A] private ( inner : Array[A] )( implicit atag : scala.reflect.ClassTag[A] ) extends ImmutableArraySeq.Abstract[A]( inner ) {
  require( !atag.runtimeClass.isPrimitive, s"Please use an implementation of ImmutableArraySeq.Abstract specialized for ${atag.runtimeClass.getName}." );
}



