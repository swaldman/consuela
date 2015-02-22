package com.mchange.sc.v1;

import com.mchange.lang.ByteUtils;

package object consuela {
  object Implicits {
    implicit class RichByteSeq( bytes : Seq[Byte] ) {
      def hex : String = ByteUtils.toLowercaseHexAscii( bytes.toArray )
    }
    implicit class RichByteArray( bytes : Array[Byte] ) {
      def hex : String = ByteUtils.toLowercaseHexAscii( bytes )
    }
  }
}
