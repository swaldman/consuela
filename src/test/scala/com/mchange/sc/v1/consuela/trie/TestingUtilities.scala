package com.mchange.sc.v1.consuela.trie;

trait TestingUtilities {
  // workaround for Scala 2.10 testing...
  implicit class RichOption[T]( opt : Option[T] ) {
    def contains( t : T ) : Boolean = {
      opt match {
        case Some( thing ) if thing == t => true
        case _                           => false
      }
    }
  }
}
