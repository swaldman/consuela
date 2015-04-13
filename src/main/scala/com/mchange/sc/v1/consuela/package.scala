package com.mchange.sc.v1;

package object consuela {
  class ConsuelaException( message : String, t : Throwable = null ) extends Exception( message, t );
}
