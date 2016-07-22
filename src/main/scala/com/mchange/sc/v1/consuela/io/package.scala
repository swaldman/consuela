package com.mchange.sc.v1.consuela;

import scala.collection._
import JavaConverters._

import java.nio.file._
import java.nio.file.attribute._
import PosixFilePermission._
import AclEntryPermission._
import AclEntryFlag._

import java.util.{ArrayList, EnumSet, List => JList}
import java.io.File

import com.mchange.sc.v2.failable._

import com.mchange.sc.v2.util.Platform

package object io {

  def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = {
    Platform.Current match {
      case Some( Platform.Mac ) | Some( Platform.Unix ) => Posix.ensureUserOnlyDirectory( path )
      case Some( Platform.Windows )                     => Windows.ensureUserOnlyDirectory( path )
      case Some( unknownPlatform )                      => fail( s"No handler for platform '${unknownPlatform}'" )
      case None                                         => fail( "Unable to detect platform in order to restrict directory access to user." )
    }
  }

  def ensureUserOnlyDirectory( file : File ) : Failable[File] = ensureUserOnlyDirectory( file.toPath ).map( _.toFile )

  def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = {
    Platform.Current match {
      case Some( Platform.Mac ) | Some( Platform.Unix ) => Posix.createUserOnlyEmptyFile( path )
      case Some( Platform.Windows )                     => Windows.createUserOnlyEmptyFile( path )
      case Some( unknownPlatform )                      => fail( s"No handler for platform '${unknownPlatform}'" )
      case None                                         => fail( "Unable to detect platform in order to restrict directory access to user." )
    }
  }

  def createUserOnlyEmptyFile( file : File ) : Failable[File] = createUserOnlyEmptyFile( file.toPath ).map( _.toFile )

  private final object Posix {
    private val AcceptableUserOnlyDirectoryPermissions = List( EnumSet.of( OWNER_READ, OWNER_WRITE, OWNER_EXECUTE ).asScala, EnumSet.of( OWNER_READ, OWNER_WRITE ).asScala )
    private val MinimalUserOnlyFileAttribute           = PosixFilePermissions.asFileAttribute( EnumSet.of( OWNER_READ, OWNER_WRITE ) )

    def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = {
      val filePermissions = Files.getPosixFilePermissions( path )

      def checkPermissions = {
        if ( AcceptableUserOnlyDirectoryPermissions.exists( _ == filePermissions ) ) {
          succeed( path )
        } else {
          fail( s"Directory '${path}' must be readable and writable only by its owner, but in fact has permissions ${filePermissions}" )
        }
      }

      def createWithPermissions = Failable( Files.createDirectory( path, MinimalUserOnlyFileAttribute ) )

      if ( Files.exists( path ) ) checkPermissions else createWithPermissions
    }

    def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = Failable {
      Files.createFile( path, MinimalUserOnlyFileAttribute )
    }
  }

  // this was very helpful: http://jakubstas.com/creating-files-and-directories-nio2/

  private final object Windows {

    private val UserOnlyDirectoryCreatePermissions = EnumSet.of( LIST_DIRECTORY, ADD_FILE, DELETE_CHILD, READ_ACL ).asScala
    private val UserOnlyFileCreatePermissions      = EnumSet.of( READ_DATA, WRITE_DATA, APPEND_DATA, DELETE, READ_ACL ).asScala

    private def findJvmUserPrincipal : Failable[UserPrincipal] = Failable {
      val fs            = FileSystems.getDefault()
      val lookupService = fs.getUserPrincipalLookupService()
      val userName      = System.getProperty("user.name")

      lookupService.lookupPrincipalByName( userName )
    }

    private def aclEntryList( userPrincipal : UserPrincipal, permissions : Set[AclEntryPermission] ) : JList[AclEntry] = {
      val builder = AclEntry.newBuilder()

      // using default flags for now
      //
      // builder.setFlags(flags)

      builder.setPermissions(permissions.asJava)
      builder.setPrincipal(userPrincipal)
      builder.setType(AclEntryType.DENY)

      val entry = builder.build()

      val out = new ArrayList[AclEntry]( 1 )
      out.add( entry )
      out
    }

    private def fileAttribute( userPrincipal : UserPrincipal, permissions : Set[AclEntryPermission] ) : FileAttribute[JList[AclEntry]] = {
      new FileAttribute[JList[AclEntry]] {
        val ael = aclEntryList( userPrincipal, permissions )

        def value() : JList[AclEntry] = ael

        def name() : String = "acl:acl"
      }
    }

    private val whitelistedPrincipalNames : Set[String] = Set("""NT AUTHORITY\SYSTEM""", """BUILTIN\Administrators""").map( _.toLowerCase )

    private def isWhitelistedPrincipal( myUserPrincipal : UserPrincipal, checkUserPrincipal : UserPrincipal ) : Boolean = {
      checkUserPrincipal == myUserPrincipal || whitelistedPrincipalNames( checkUserPrincipal.getName().toLowerCase )
    }

    def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = {

      findJvmUserPrincipal.flatMap { userPrincipal =>
        val fa = fileAttribute( userPrincipal, UserOnlyDirectoryCreatePermissions )
        if ( !Files.exists( path ) ) {
          Failable( Files.createDirectory( path, fa ) )
        } else {
          val view = Files.getFileAttributeView(path, classOf[AclFileAttributeView])
          val acls = view.getAcl().asScala
          val badPrincipals = acls.filter( acl => !isWhitelistedPrincipal( userPrincipal, acl.principal ) )
          if ( badPrincipals.size == 0 ) succeed ( path ) else fail( "${path} should be a user-only directory, but is accessible by ${badPrincipals.mkString}." )
        }
      }
    }

    def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable( Files.createFile( path, fileAttribute( userPrincipal, UserOnlyFileCreatePermissions ) ) )
      }
    }

  }
}
