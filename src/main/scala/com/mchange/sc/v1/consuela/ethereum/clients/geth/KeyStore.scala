package com.mchange.sc.v1.consuela.ethereum.clients.geth

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{wallet,EthAddress}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20

import com.mchange.sc.v2.failable._
import com.mchange.sc.v2.lang.borrow
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import java.io.{BufferedOutputStream,File,FileOutputStream}

import scala.collection.immutable

final object KeyStore {

  private val TimestampPattern = "yyyy-MM-dd'T'HH-mm-ss.SSS"
  private val TimeZone         = java.util.TimeZone.getTimeZone("UTC")

  private val DirName = "keystore"

  private val BufferSize = 2048

  private val FilenameRegex = """^UTC\-\-.*\-\-([1234567890abcdefABCDEF]{40})$""".r

  private def extraDigits( n : Int ) : String = (0 to n).map( _ => scala.util.Random.nextInt(10) ).mkString("")

  private def generateWalletFileName( address : EthAddress ) : String = {
    def filename( timestamp : String, addressHexNoPrefix : String ) : String = s"UTC--${timestamp}${extraDigits(5)}Z--${addressHexNoPrefix}"

    val df = new java.text.SimpleDateFormat(KeyStore.TimestampPattern)
    df.setTimeZone(KeyStore.TimeZone)
    filename( df.format( new java.util.Date() ), address.bytes.widen.hex )
  }

  private def parseAddress( fname : String ) : Option[EthAddress] = {
    fname match {
      case FilenameRegex( hexChars ) => Some( EthAddress( ByteSeqExact20( hexChars.decodeHex ) ) )
      case _                         => None
    }
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

  def listAddresses() : Failable[immutable.Seq[EthAddress]] = directory.map( dir => ImmutableArraySeq.createNoCopy( dir.list.map( parseAddress ).filter( _.isDefined ).map( _.get ) ) )

  def walletForAddress( dir : File, address : EthAddress ) : Failable[wallet.V3] = _walletForAddress( succeed( dir ), address )

  def walletForAddress( address : EthAddress ) : Failable[wallet.V3] = _walletForAddress( directory, address )

  private def _walletForAddress( dir : Failable[File], address : EthAddress ) : Failable[wallet.V3] = {
    val goodFileNames = directory.map( d => d.list.filter( fname => parseAddress( fname ).isDefined ).filter( _.toLowerCase.endsWith( address.bytes.widen.hex.toLowerCase ) ) )

    val goodName = {
      goodFileNames.flatMap { names =>
        if ( names.length == 1 ) succeed( names.head )
        else if ( names.length == 0 ) fail( s"Wallet for address 0x${address.bytes.widen.hex} not found." )
        else fail ( s"""Multiple wallets found for address 0x${address.bytes.widen.hex} -- ${names.mkString(",")}""" )
      }
    }
    val file = goodName.flatMap( name => directory.map( dir => new File( dir, name ) ) )

    file.map( wallet.V3.apply )
  }

  lazy val directory : Failable[File] = {
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

