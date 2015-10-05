package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable;

object Session {

  val Bootstrap = Session( Subprotocol.Known( (Subprotocol.Name.Base, 0) ) );

  def compute( counterpartyTuples : Set[(String,Int)] ) : Session = symmetricCompute( Subprotocol.Known.keySet, counterpartyTuples )

  private def symmetricCompute( a : Set[(String,Int)], b : Set[(String,Int)] ) : Session = {
    val orderedBestShared : Set[(String,Int)] = {
      def pairSetToIntSet( pairSet : Set[(String,Int)] ) : Set[Int] = pairSet.map( _._2 )
      val ma = a.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val mb = b.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val unordered : Map[String,Int] = ma.filterKeys( mb.contains( _ ) ).map( pair => Tuple2[String,Int]( pair._1, (ma( pair._1 ) intersect mb( pair._1 )).max ) )
      immutable.TreeSet( unordered.toSeq : _* )
    }
    val basedBestShared = orderedBestShared + Subprotocol.BaseTuple
    Session( basedBestShared.foldLeft( immutable.IndexedSeq.empty[Subprotocol.Info] )( ( seq, tuple ) => seq ++ Subprotocol.Known( tuple ) ) )
  }
}
case class Session( infoByTypeCode : immutable.IndexedSeq[Subprotocol.Info] ) {
  def infoByNameAndOffset( name : String, offset : Int ) : Subprotocol.Info = {
    infoByTypeCode.find( info => info.name == name && info.offset == offset ).get //throw Exception if we are looking up a nonexistent type
  }

  def typeCodeByNameAndOffset( name : String, offset : Int ) : Int = {
    infoByTypeCode.indexOf( infoByNameAndOffset( name, offset ) )
  }
}

