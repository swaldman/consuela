package com.mchange.sc.v1.consuela.hash;

import scala.language.reflectiveCalls;

object Hasher {
  trait FixedLength[T <: Hash[T]] extends Abstract[T]{
    val HashLength : Int; 

    lazy val Zero = instantiate( Array.ofDim[Byte]( HashLength ) );

    private lazy val Ensurer : T => Boolean = _.length == HashLength

    override def withBytes( bytes : Array[Byte] ) : T = {
      require( bytes.length == HashLength, badLenMessage( bytes ) )
      super.withBytes( bytes )
    }
    override def withBytes( bytes : Seq[Byte] ) : T = {
      require( bytes.length == HashLength, badLenMessage( bytes ) )
      super.withBytes( bytes )
    }
    override def hash( bytes : Array[Byte] ) : T = super.hash( bytes ) ensuring( Ensurer, badLenMessage( bytes ) );
    override def hash( bytes : Seq[Byte] ) : T   = super.hash( bytes ) ensuring( Ensurer, badLenMessage( bytes ) );

    private def badLenMessage( bytes : { def length : Int } ) = s"A ${AlgoName} hash must have a fixed length of ${HashLength} bytes, cannot create with ${bytes.length} bytes. bytes -> ${bytes}"
  }
  abstract class Abstract[T <: Hash[T]] extends Hasher[T] {
    val AlgoName : String;
    protected def instantiate( bytes : Array[Byte] ) : T;

    def withBytes( bytes : Array[Byte] ) : T = instantiate( bytes.clone() );
    def withBytes( bytes : Seq[Byte] ) : T   = instantiate( bytes.toArray );
    def hash( bytes : Array[Byte] ) : T      = instantiate( doHash(AlgoName, bytes) );
    def hash( bytes : Seq[Byte] ) : T        = instantiate( doHash(AlgoName, bytes) );

    def rawHash( bytes : Array[Byte] ) : Array[Byte] = doHash(AlgoName, bytes)
  }
}

trait Hasher[T <: Hash[T]] {
  def withBytes( bytes : Seq[Byte] ) : T;
  def withBytes( bytes : Array[Byte] ) : T;
  def Zero : T;

  def hash( bytes : Seq[Byte] )   : T;
  def hash( bytes : Array[Byte] ) : T;

  def rawHash( bytes : Array[Byte] ) : Array[Byte];
}
