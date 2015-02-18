package com.mchange.sc.v1.consuela.hash;

trait Hasher[T <: Hash[T]] {
  def apply( bytes : Seq[Byte] ) : T;
  def apply( bytes : Array[Byte] ) : T;
  def Zero : T;
}
