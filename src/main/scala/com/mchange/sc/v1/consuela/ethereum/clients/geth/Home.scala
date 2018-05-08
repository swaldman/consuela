package com.mchange.sc.v1.consuela.ethereum.clients.geth

import java.io.File

import com.mchange.sc.v3.failable._

import com.mchange.sc.v2.util.Platform

object Home {
  val Directory = {
    val Tag = "clients.geth.Home.Directory"
    
    Platform.Current match {
      case Some( Platform.Windows ) => Platform.Windows.appSupportDirectory( "Ethereum" ) // underneath %APPDATA%
      case Some( Platform.Mac )     => Option( System.getProperty("user.home") ).map( home => new File( s"${home}/Library/Ethereum" ) ).toFailable(s"${Tag}: On Mac, but could not find System property 'user.home'")
      case Some( Platform.Unix )    => Platform.Unix.appSupportDirectory( "ethereum" ) // makes a dotfile directory
      case Some( unknown )          => Failable.fail( s"Uncertain of appropriate geth home directory for Platform ${unknown}" )
      case None                     => Failable.fail( "Could not detect platform to determine the location of the geth home directory" )
    }
  }
}
