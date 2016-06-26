package com.mchange.sc.v1.consuela.ethereum.clients.geth

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{wallet,EthAddress}

import com.mchange.sc.v2.failable._
import com.mchange.sc.v2.lang.borrow

import java.io.{BufferedOutputStream,File,FileOutputStream}

final object KeyStore {
  private val TimestampPattern = "yyyy-MM-dd'T'HH-mm-ss.SSS"
  private val TimeZone         = java.util.TimeZone.getTimeZone("UTC")

  private val DirName = "keystore"

  private val BufferSize = 2048

  private def extraDigits( n : Int ) : String = (0 to n).map( _ => scala.util.Random.nextInt(10) ).mkString("")

  private def generateWalletFileName( address : EthAddress ) : String = {
    def filename( timestamp : String, addressHexNoPrefix : String ) : String = s"UTC--${timestamp}${extraDigits(5)}Z--${addressHexNoPrefix}"

    val df = new java.text.SimpleDateFormat(KeyStore.TimestampPattern)
    df.setTimeZone(KeyStore.TimeZone)
    filename( df.format( new java.util.Date() ), address.bytes.widen.hex )
  }

  def addNew( passphrase : String ) : Failable[wallet.V3] = {
    Failable {
      val w = wallet.V3.generateScrypt( passphrase )
      directory.map { dir =>
        borrow( new BufferedOutputStream( new FileOutputStream( new File( dir, generateWalletFileName( w.address ) ) ), BufferSize ) ){ os =>
          w.write( os )
          w
        }
      }
    }.flatten
  }

  lazy val directory : Failable[java.io.File] = {
    val osName = Option( System.getProperty("os.name") ).map( _.toLowerCase ).toFailable("geth.Keystore.directory: Couldn't detect OS, System property 'os.name' not available.")
    osName.flatMap { osn =>
      if ( osn.indexOf( "win" ) >= 0 ) {
        Option( System.getenv("APPDATA") ).map( ad => new java.io.File(ad, DirName) ).toFailable("geth.Keystore.directory: On Windows, but could not find environment variable 'APPDATA'")
      } else if ( osn.indexOf( "mac" ) >= 0 ) {
        Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/Library/Ethereum", DirName) ).toFailable("geth.Keystore.directory: On Mac, but could not find System property 'user.home'")
      } else {
        Option( System.getProperty("user.home") ).map( home => new java.io.File( s"${home}/.ethereum", DirName) ).toFailable("geth.Keystore.directory: On Unix, but could not find System property 'user.home'")
      }
    }
  }
}

