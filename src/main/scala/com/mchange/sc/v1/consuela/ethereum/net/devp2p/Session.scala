package com.mchange.sc.v1.consuela.ethereum.net.devp2p;

import scala.collection.immutable

import com.mchange.sc.v1.consuela.ethereum.specification.Types._

object Session {

  val Bootstrap = Session( immutable.IndexedSeq( Subprotocol.P2P4 ) );

  def compute( counterpartyTuples : Set[(String,Int)] ) : Session = symmetricCompute( Subprotocol.All.map( _.WideIdentifier ).toSet, counterpartyTuples )

  private def symmetricCompute( a : Set[(String,Int)], b : Set[(String,Int)] ) : Session = {
    val orderedBestShared : Seq[(StringASCII_Exact3,Unsigned16)] = {
      def pairSetToIntSet( pairSet : Set[(String,Int)] ) : Set[Int] = pairSet.map( _._2 )
      val ma = a.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val mb = b.groupBy( _._1 ).mapValues( pairSetToIntSet );
      val unordered : Map[String,Int] = ma.filterKeys( mb.contains( _ ) ).map( pair => Tuple2[String,Int]( pair._1, (ma( pair._1 ) intersect mb( pair._1 )).max ) )
      val unorderedTyped = unordered.map( tup => ( StringASCII_Exact3( tup._1 ), Unsigned16( tup._2 ) ) )

      // we have to put the P2P tuple (if present) first
      val mbP2PTuple = unorderedTyped.find( _._1 == Subprotocol.P2P4.Name )
      mbP2PTuple.fold( unorderedTyped.toSeq )( P2PTuple => P2PTuple +: unorderedTyped.toSeq.filter( _._1 != Subprotocol.P2P4.Name ) )
    }
    Session( orderedBestShared.foldLeft( immutable.IndexedSeq.empty[Subprotocol] )( ( seq, tuple ) => seq :+ Subprotocol.byIdentifier( tuple ) ) )
  }
}
case class Session( orderedSubprotocols : immutable.IndexedSeq[Subprotocol] ) {

  lazy val payloadFactories : IndexedSeq[ Payload.Factory[_] ] = orderedSubprotocols.flatMap( _.PayloadFactories )

  private lazy val startIndices : Map[ StringASCII_Exact3, Int ] = payloadFactories.zipWithIndex.groupBy( _._1.subprotocol.Name ).mapValues( seq => seq.map( _._2 ).sorted.head )

  def typeCodeByNameAndOffset( name : StringASCII_Exact3, offset : Int ) : Int = startIndices( name ) + offset
}

