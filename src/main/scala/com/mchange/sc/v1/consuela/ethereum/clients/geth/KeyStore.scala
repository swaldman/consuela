package com.mchange.sc.v1.consuela.ethereum.clients.geth

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.io.createReadOnlyFile
import com.mchange.sc.v1.consuela.ethereum.{wallet,EthAddress}
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20

import com.mchange.sc.v3.failable._
import com.mchange.sc.v2.lang.borrow
import com.mchange.sc.v2.collection.immutable.ImmutableArraySeq

import java.io.File

import scala.collection._

final object KeyStore {

  private val TimestampPattern = "yyyy-MM-dd'T'HH-mm-ss.SSS"
  private val TimeZone         = java.util.TimeZone.getTimeZone("UTC")

  private val DirName = "keystore"

  private val BufferSize = 2048

  private val FilenameRegex = """^UTC\-\-.*\-\-([1234567890abcdefABCDEF]{40})$""".r

  private def extraDigits( n : Int ) : String = (0 to n).map( _ => scala.util.Random.nextInt(10) ).mkString("")

  def generateWalletFileName( address : EthAddress ) : String = {
    def filename( timestamp : String, addressHexNoPrefix : String ) : String = s"UTC--${timestamp}${extraDigits(5)}Z--${addressHexNoPrefix}"

    val df = new java.text.SimpleDateFormat(KeyStore.TimestampPattern)
    df.setTimeZone(KeyStore.TimeZone)
    filename( df.format( new java.util.Date() ), address.hex )
  }

  private def parseAddress( fname : String ) : Option[EthAddress] = {
    fname match {
      case FilenameRegex( hexChars ) => Some( EthAddress( ByteSeqExact20( hexChars.decodeHex ) ) )
      case _                         => None
    }
  }

  def addNew( passphrase : String ) : Failable[wallet.V3] = Directory.flatMap( addNew( _, passphrase ) )

  def addNew( dir : File, passphrase : String ) : Failable[wallet.V3] = add( dir, wallet.V3.generateScrypt( passphrase ) )

  def add( dir : File, w : wallet.V3 ) : Failable[wallet.V3] = Failable {
    val newFile = new File( dir, generateWalletFileName( w.address ) )
    if (!newFile.exists()) {
      createReadOnlyFile( newFile, w.toByteArray ).map( _ => w )
    } else {
      throw new Exception( s"Huh? The timestamped, somewhat randomly named file we are creating (${newFile}) already exists?" )
    }
  }.flatten

  def listAddresses() : Failable[immutable.Seq[EthAddress]] = Directory.map( dir => ImmutableArraySeq.createNoCopy( dir.list.map( parseAddress ).filter( _.isDefined ).map( _.get ) ) )

  def walletsForAddress( dir : File, address : EthAddress ) : Failable[immutable.Set[wallet.V3]] = _walletsForAddress( Failable.succeed( dir ), address )

  def walletsForAddress( address : EthAddress ) : Failable[immutable.Set[wallet.V3]] = _walletsForAddress( Directory, address )

  private def _walletsForAddress( dir : Failable[File], address : EthAddress ) : Failable[immutable.Set[wallet.V3]] = {
    val goodFileNames : Failable[Array[String]] = dir.map( d => d.list.filter( fname => parseAddress( fname ).isDefined ).filter( _.toLowerCase.endsWith( address.bytes.widen.hex.toLowerCase ) ) )
    val goodFiles     : Failable[Array[File]]   = goodFileNames.flatMap( names => dir.map( d => names.map( name => new File( d, name ) ) ) )
    goodFiles.map( arr => arr.map( wallet.V3.apply ).toSet )
  }

  lazy val Directory : Failable[File] = Home.Directory.map( home => new File( home, DirName ) )
}

