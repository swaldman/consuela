package com.mchange.sc.v1.consuela.ethereum.jsonrpc20.playws

import scala.collection._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import com.mchange.sc.v1.log.MLevel._

object Manager {
  final class ClosedException( mgr : Manager ) extends Exception( s"Operation failed: ${mgr} has already been closed()." )
}
class Manager extends AutoCloseable {
  implicit val akkaSystem = ActorSystem( this.toString )
  val materializer = ActorMaterializer()

  def close() : Unit = {
    closeChildren()
    akkaSystem.terminate()
  }

  // MT: protected by this' lock
  private var children = mutable.HashSet.empty[Exchanger]

  def register( exchanger : Exchanger )   : Unit = this.synchronized( if ( children != null ) children += exchanger else throw new Manager.ClosedException(this) )
  def deregister( exchanger : Exchanger ) : Unit = this.synchronized( if ( children != null ) children -= exchanger else throw new Manager.ClosedException(this) )
  def closeChildren() : Unit = this.synchronized{
    children.foreach( child => WARNING.attemptWithLabel( s"Attempt by ${this} to autoclose unclosed ${classOf[Exchanger].getName} failed" )( child.close() ) )
    children = null
  }
}
