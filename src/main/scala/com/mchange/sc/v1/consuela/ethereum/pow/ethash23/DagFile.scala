package com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import com.mchange.sc.v1.consuela.ethereum.{EthereumException};

import com.mchange.sc.v1.consuela._
import conf.Config;

import com.mchange.lang.LongUtils;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import java.io.File;
import java.nio.file.attribute.PosixFilePermission;

// see https://github.com/ethereum/wiki/wiki/Ethash-DAG
object DagFile {
  private implicit lazy val logger = MLogger( this );

  val BufferSize = 16 * 1024 * 1024; // 16MB

  val PosixCacheDirPermissions = {
    val jhs = new java.util.HashSet[PosixFilePermission];
    jhs.add( PosixFilePermission.OWNER_READ );
    jhs.add( PosixFilePermission.OWNER_WRITE );
    jhs.add( PosixFilePermission.OWNER_EXECUTE );
    jhs
  }
  val PosixCacheFilePermissions = {
    val jhs = new java.util.HashSet[PosixFilePermission];
    jhs.add( PosixFilePermission.OWNER_READ );
    jhs.add( PosixFilePermission.OWNER_WRITE );
    jhs
  }

  private[ethash23] final val MagicNumber = 0xFEE1DEADBADDCAFEL
  private[ethash23] final val MagicNumberLittleEndianBytes = {
    val bytes = Array.ofDim[Byte](8);
    LongUtils.longIntoByteArrayLittleEndian( MagicNumber, 0, bytes );
    bytes
  }

  def nameForSeed( seed : Array[Byte] ) : String = s"full-R${Revision}-${seed.take(8).hex}"

  def fileForSeed( seed : Array[Byte] ) : File = new File( ConfiguredDirectory, nameForSeed( seed ) );

  lazy val ConfiguredDirectory = Config.EthereumPowEthash23DagFileDirectory

    val IsWindows : Boolean = {
      val osName = {
        val tmp = System.getProperty( "os.name" );
        if ( tmp == null ) {
          WARNING.log("Could not find a value for System property 'os.name'. Will presume UNIX-like.");
          ""
        } else {
          tmp.toLowerCase
        }
      }
      osName.indexOf("win") >= 0;
    }
    val isPosix = !IsWindows;
    val DefaultDirectory : String = {
      def homeless( dflt : String ) : String = {
        WARNING.log(s"Could not find a value for System property 'user.home'. Using default directory '${dflt}'.");
        dflt
      }
      val homeDir = {
        val userHome = System.getProperty( "user.home" );
        ( IsWindows, userHome ) match {
          case ( true, null )  => homeless("C:\\TEMP")
          case ( false, null ) => homeless("/tmp")
          case _               => userHome
        }
      }
      def windowsDir = s"${homeDir}\\Appdata\\Ethash";
      def unixDir = s"${homeDir}/.ethash";
      if ( IsWindows ) windowsDir else unixDir;
    }

    class BadMagicNumberException private[ethash23] ( message : String, t : Throwable = null ) extends EthereumException( message, t );
  }

