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

  def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.ensureUserOnlyDirectory( path ) )
  def ensureUserOnlyDirectory( file : File ) : Failable[File] = ensureUserOnlyDirectory( file.toPath ).map( _.toFile )

  def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.createUserOnlyEmptyFile( path ) )
  def createUserOnlyEmptyFile( file : File ) : Failable[File] = createUserOnlyEmptyFile( file.toPath ).map( _.toFile )

  def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.createUserReadOnlyEmptyFile( path ) )
  def createUserReadOnlyEmptyFile( file : File ) : Failable[File] = createUserReadOnlyEmptyFile( file.toPath ).map( _.toFile )

  private def doWithPlatformHelper( path : Path )( f : (Path, UserOnlyHelper ) => Failable[Path] ) : Failable[Path] = {
    Platform.Current match {
      case Some( Platform.Mac ) | Some( Platform.Unix ) => f( path, Posix )
      case Some( Platform.Windows )                     => f( path, Windows )
      case Some( unknownPlatform )                      => fail( s"No handler for platform '${unknownPlatform}'" )
      case None                                         => fail( "Unable to detect platform in order to restrict directory access to user." )
    }
  }

  private trait UserOnlyHelper {
    def ensureUserOnlyDirectory( path : Path )     : Failable[Path]
    def createUserOnlyEmptyFile( path : Path )     : Failable[Path]
    def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path]
  }

  private final object Posix extends UserOnlyHelper {
    private val AcceptableUserOnlyDirectoryPermissions = List( EnumSet.of( OWNER_READ, OWNER_WRITE, OWNER_EXECUTE ).asScala, EnumSet.of( OWNER_READ, OWNER_WRITE ).asScala )
    private val UserOnlyFileAttribute                  = PosixFilePermissions.asFileAttribute( EnumSet.of( OWNER_READ, OWNER_WRITE ) )
    private val UserReadOnlyFileAttribute              = PosixFilePermissions.asFileAttribute( EnumSet.of( OWNER_READ ) )

    def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = {
      val filePermissions = Files.getPosixFilePermissions( path )

      def checkPermissions = {
        if ( AcceptableUserOnlyDirectoryPermissions.exists( _ == filePermissions ) ) {
          succeed( path )
        } else {
          fail( s"Directory '${path}' must be readable and writable only by its owner, but in fact has permissions ${filePermissions}" )
        }
      }

      def createWithPermissions = Failable( Files.createDirectory( path, UserOnlyFileAttribute ) )

      if ( Files.exists( path ) ) checkPermissions else createWithPermissions
    }

    def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = Failable {
      Files.createFile( path, UserOnlyFileAttribute )
    }

    def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path] = Failable {
      Files.createFile( path, UserReadOnlyFileAttribute )
    }
  }

  // this was very helpful: http://jakubstas.com/creating-files-and-directories-nio2/

  private final object Windows extends UserOnlyHelper {

    private val UserOnlyDirectoryCreatePermissions = EnumSet.of( LIST_DIRECTORY, ADD_FILE, DELETE_CHILD, READ_ACL ).asScala
    private val UserOnlyFileCreatePermissions      = EnumSet.of( READ_DATA, WRITE_DATA, APPEND_DATA, DELETE, READ_ACL ).asScala
    private val UserReadOnlyFileCreatePermissions  = EnumSet.of( READ_DATA, READ_ACL ).asScala

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

    def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable( Files.createFile( path, fileAttribute( userPrincipal, UserReadOnlyFileCreatePermissions ) ) )
      }
    }
  }
}
