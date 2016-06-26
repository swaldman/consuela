package com.mchange.sc.v1.consuela.ethereum.clients.geth

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.EthAddress

final object KeyStore {
  private val TimestampPattern = "yyyy-MM-dd'T'HH-mm-ss.SSS"
  private val TimeZone         = java.util.TimeZone.getTimeZone("UTC")

  private val DirName = "keystore"

  def extraDigits( n : Int ) : String = (0 to n).map( _ => scala.util.Random.nextInt(10) ).mkString("")

  def generateFileName( address : EthAddress ) : String = {
    def filename( timestamp : String, addressHexNoPrefix : String ) : String = s"UTC--${timestamp}${extraDigits(5)}Z--${addressHexNoPrefix}"

    val df = new java.text.SimpleDateFormat(KeyStore.TimestampPattern)
    df.setTimeZone(KeyStore.TimeZone)
    filename( df.format( new java.util.Date() ), address.bytes.widen.hex )
  }

  lazy val directory : Option[java.io.File] = {
    val osName = Option( System.getProperty("os.name") ).map( _.toLowerCase )
    osName.flatMap { osn =>
      if ( osn.indexOf( "win" ) >= 0 ) {
        Option( System.getenv("APPDATA") ).map( ad => new java.io.File(ad, DirName) )
      } else if ( osn.indexOf( "mac" ) >= 0 ) {
        Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/Library/Ethereum", DirName) )
      } else {
        Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/.ethereum", DirName) )
      }
    }
  }
}

