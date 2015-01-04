package com.mchange.sc.v1.consuela.ethereum;

import java.nio.charset.Charset;

import com.mchange.lang.ByteUtils;

import org.specs2._;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

class RLPSpec extends Specification { 

  private[this] implicit val logger = MLogger( this );
  private[this] implicit val charset = Charset.forName("UTF-8");

  def is =
s2"""
   RLP should
         encode empty string to 0x80                ${ e1 }
         encode dog to 0x83646f67                   ${ e2 }
         properly encode "Lorem ipsum..."           ${ e3 }
""";

  def b( i : Int ) : Byte = i.asInstanceOf[Byte];

  val E1 = Seq( b(0x80) );
  val E2 = ByteUtils.fromHexAscii("83646f67").toSeq;
  val E3 = ByteUtils.fromHexAscii("b74c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e7365637465747572206164697069736963696e6720656c69").toSeq;

  def e1 : Boolean = RLP.encode( Seq.empty[Byte] ) == E1
  def e2 : Boolean = RLP.encode( "dog" ) == E2
  def e3 : Boolean = RLP.encode( "Lorem ipsum dolor sit amet, consectetur adipisicing eli" ) == E3


/*
  def e2 : Boolean = {
    printEval( RLP.encode( 0x7f.asInstanceOf[Byte] ) ) match {
      case Seq( b0, b1 ) => b0 == 0x81 && b1 == 0x7f
      case _             => false
    }
  }
 */

  def printEval[T]( expr : =>T ) : T = { val out = expr; println( "===> " + out ); out }
}
