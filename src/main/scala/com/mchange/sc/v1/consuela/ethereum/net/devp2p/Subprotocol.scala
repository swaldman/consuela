package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable;
import com.mchange.sc.v2.failable._


object Subprotocol {
  final object Name {
    val Base = "!base"; // exclamation point so that Base's name comes first in standard orderings
    val Eth  = "eth";
    val Shh  = "shh";
  }

  final object Info {
    val Base0 : immutable.IndexedSeq[Info] = (0x00 until 0x10).map( i => Info( Name.Base, 0, i, _ => throw new Exception, _ => throw new Exception ) )
  }
  final case class Info( name : String, version : Int, offset : Int, pickle : Payload => Failable[immutable.Seq[Byte]], unpickle : Seq[Byte] => Payload )

  val Known : Map[ (String, Int), immutable.IndexedSeq[Info] ] = immutable.Map(
    ( Name.Base, 0 ) -> Info.Base0
  )

  val BaseTuple = ( Name.Base, 0 )
}
