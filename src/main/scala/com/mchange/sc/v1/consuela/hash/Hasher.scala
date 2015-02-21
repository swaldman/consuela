package com.mchange.sc.v1.consuela.hash;

object Hasher {
  trait FixedLength {
    self : Hasher[_] =>

    val HashLength : Int; 
  }
}

trait Hasher[T <: Hash[T]] {
  def withBytes( bytes : Seq[Byte] ) : T;
  def withBytes( bytes : Array[Byte] ) : T;
  def Zero : T;

  def hash( bytes : Seq[Byte] )   : T;
  def hash( bytes : Array[Byte] ) : T;
}
