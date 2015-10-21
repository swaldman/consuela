package com.mchange.sc.v1.consuela.ethereum.net;

import scala.collection.immutable;
import com.mchange.sc.v2.restrict._;

package object rlpx {

  final object Types {
    final object Ecies256PublicKeyUncompressed extends RestrictedType[Nothing,immutable.Seq[Byte],Ecies256PublicKeyUncompressed] {
      val UncompressedMarker : Byte = 0x04.toByte // the last byte should signify uncompressed
      protected def create( bytes : immutable.Seq[Byte] ) : Ecies256PublicKeyUncompressed = Ecies256PublicKeyUncompressed( bytes );
      protected def pretransform( bytes : immutable.Seq[Byte] ) : immutable.Seq[Byte] = if (bytes.length == 64) bytes :+ UncompressedMarker else bytes
      def contains( bytes : immutable.Seq[Byte] ) : Boolean = bytes.length == 65 && bytes(64) == UncompressedMarker
    }
    final class Ecies256PublicKeyUncompressed private ( val widen : immutable.Seq[Byte] ) extends AnyVal with RestrictedType.Element[immutable.Seq[Byte]]
  }

/*
  case class InitiatorHandshakeMessage (

  )
*/ 
}
    
