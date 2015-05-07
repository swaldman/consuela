package com.mchange.sc.v1.consuela.hash;

import com.mchange.sc.v1.consuela.util.ByteArrayValue;

trait Hash[T <: Hash[T]] extends ByteArrayValue with ByteArrayValue.UnsignedBigIntegral {
  def hasher : Hasher[T];
}
