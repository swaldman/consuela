package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable;

object Session {

  val Bootstrap = Session( immutable.IndexedSeq( Subprotocol.Core ) );

  def compute( counterpartyTuples : Set[(String,Int)] ) : Session = symmetricCompute( Subprotocol.All.map( _.Identifier ).toSet, counterpartyTuples )

  private def symmetricCompute( a : Set[(String,Int)], b : Set[(String,Int)] ) : Session = {
    val orderedBestShared : Set[(String,Int)] = {
      def pairSetToIntSet( pairSet : Set[(String,Int)] ) : Set[Int] = pairSet.map( _._2 )
      val ma = a.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val mb = b.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val unordered : Map[String,Int] = ma.filterKeys( mb.contains( _ ) ).map( pair => Tuple2[String,Int]( pair._1, (ma( pair._1 ) intersect mb( pair._1 )).max ) )
      immutable.TreeSet( unordered.toSeq : _* )
    }
    val basedBestShared = orderedBestShared + Subprotocol.Core.Identifier
    Session( basedBestShared.foldLeft( immutable.IndexedSeq.empty[Subprotocol] )( ( seq, tuple ) => seq :+ Subprotocol.byIdentifier( tuple ) ) )
  }
}
case class Session( orderedSubprotocols : immutable.IndexedSeq[Subprotocol] ) {

  lazy val payloadFactories : IndexedSeq[ Payload.Factory[_] ] = orderedSubprotocols.flatMap( _.PayloadFactories )

  private lazy val startIndices : Map[ String, Int ] = payloadFactories.zipWithIndex.groupBy( _._1.subprotocol.Name ).mapValues( seq => seq.map( _._2 ).sorted.head )

  def typeCodeByNameAndOffset( name : String, offset : Int ) : Int = startIndices( name ) + offset
}

