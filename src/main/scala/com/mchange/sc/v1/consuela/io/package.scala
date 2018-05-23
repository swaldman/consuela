package com.mchange.sc.v1.consuela;

import scala.collection._
import JavaConverters._

import java.nio.file._
import java.nio.file.attribute._
import PosixFilePermission._
import AclEntryPermission._
import AclEntryFlag._
import StandardOpenOption._

import java.util.{ArrayList, EnumSet, List => JList}
import java.io.File

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v3.failable._

import com.mchange.sc.v2.util.Platform

package object io {

  def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.ensureUserOnlyDirectory( path ) )
  def ensureUserOnlyDirectory( file : File ) : Failable[File] = ensureUserOnlyDirectory( file.toPath ).map( _.toFile )

  def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.createUserOnlyEmptyFile( path ) )
  def createUserOnlyEmptyFile( file : File ) : Failable[File] = createUserOnlyEmptyFile( file.toPath ).map( _.toFile )

  def setUserReadOnlyFilePermissions( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.setUserReadOnlyFilePermissions( path ) )
  def setUserReadOnlyFilePermissions( file : File ) : Failable[File] = setUserReadOnlyFilePermissions( file.toPath ).map( _.toFile )

  def setUserOnlyFilePermissions( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.setUserOnlyFilePermissions( path ) )
  def setUserOnlyFilePermissions( file : File ) : Failable[File] = setUserOnlyFilePermissions( file.toPath ).map( _.toFile )

  def setUserOnlyDirectoryPermissions( path : Path ) : Failable[Path] = doWithPlatformHelper( path )( ( path, helper ) => helper.setUserOnlyDirectoryPermissions( path ) )
  def setUserOnlyDirectoryPermissions( file : File ) : Failable[File] = setUserOnlyDirectoryPermissions( file.toPath ).map( _.toFile )

  def createReadOnlyFile( path : Path, bytes : Array[Byte] ) : Failable[Path] = {
    for {
      emptyFilePath  <- createUserOnlyEmptyFile( path )
      fullFilePath   <- fillPath( emptyFilePath, bytes )
      sealedFilePath <- setUserReadOnlyFilePermissions( fullFilePath )
    } yield {
      sealedFilePath
    }
  }
  def createReadOnlyFile( file : File, bytes : Array[Byte] ) : Failable[File] = createReadOnlyFile( file.toPath, bytes ).map( _.toFile )

  def fillPath( path : Path, bytes : Array[Byte] ) : Failable[Path] = Failable {
    borrow( Files.newOutputStream( path, WRITE, APPEND ) )( _.write( bytes ) )
    path
  }

  private def doWithPlatformHelper( path : Path )( f : (Path, UserOnlyHelper ) => Failable[Path] ) : Failable[Path] = {
    Platform.Current match {
      case Some( Platform.Mac ) | Some( Platform.Unix ) => f( path, Posix )
      case Some( Platform.Windows )                     => f( path, Windows )
      case Some( unknownPlatform )                      => Failable.fail( s"No handler for platform '${unknownPlatform}'" )
      case None                                         => Failable.fail( "Unable to detect platform in order to restrict directory access to user." )
    }
  }

  private trait UserOnlyHelper {
    def ensureUserOnlyDirectory( path : Path )         : Failable[Path]
    def createUserOnlyEmptyFile( path : Path )         : Failable[Path]
    def setUserReadOnlyFilePermissions( path : Path )  : Failable[Path]
    def setUserOnlyFilePermissions( path : Path )      : Failable[Path]
    def setUserOnlyDirectoryPermissions( path : Path ) : Failable[Path]
  }

  private final object Posix extends UserOnlyHelper {
    private val UserOnlyDirectoryPermissions = EnumSet.of( OWNER_READ, OWNER_WRITE, OWNER_EXECUTE )
    private val UserOnlyDirectoryAttribute   = PosixFilePermissions.asFileAttribute( UserOnlyDirectoryPermissions )
    private val UserOnlyFileAttribute        = PosixFilePermissions.asFileAttribute( EnumSet.of( OWNER_READ, OWNER_WRITE ) )
    private val UserReadOnlyFilePermissions  = EnumSet.of( OWNER_READ )
    private val UserOnlyFilePermission       = EnumSet.of( OWNER_READ, OWNER_WRITE )

    def ensureUserOnlyDirectory( path : Path ) : Failable[Path] = {

      def checkPermissions = {
        val filePermissions = Files.getPosixFilePermissions( path )
        if ( UserOnlyDirectoryPermissions.asScala == filePermissions.asScala ) { // use asScala, so we are doing a value rather than identity check
          Failable.succeed( path )
        } else {
          Failable.fail( s"Directory '${path}' must be readable and writable only by its owner, but in fact has permissions ${filePermissions}" )
        }
      }

      def createWithPermissions = Failable( Files.createDirectory( path, UserOnlyDirectoryAttribute ) )

      if ( Files.exists( path ) ) checkPermissions else createWithPermissions
    }

    def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = Failable {
      Files.createFile( path, UserOnlyFileAttribute )
    }

    //def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path] = Failable {
    //  Files.createFile( path, UserReadOnlyFileAttribute )
    //}

    def setUserReadOnlyFilePermissions( path : Path ) : Failable[Path] = Failable {
      Files.setPosixFilePermissions( path, UserReadOnlyFilePermissions )
      path
    }

    def setUserOnlyFilePermissions( path : Path ) : Failable[Path] = Failable {
      Files.setPosixFilePermissions( path, UserOnlyFilePermission )
      path
    }

    def setUserOnlyDirectoryPermissions( path : Path ) : Failable[Path] = Failable {
      Files.setPosixFilePermissions( path, UserOnlyDirectoryPermissions )
      path
    }


  }

  // this was very helpful: http://jakubstas.com/creating-files-and-directories-nio2/

  private final object Windows extends UserOnlyHelper {

    // XXX: Remove once warned deficiency regarding read-only files is fixed
    import com.mchange.sc.v1.log.MLevel._

    // XXX: Remove once warned deficiency regarding read-only files is fixed
    implicit lazy val logger = mlogger( this )

    /*
     *  maybe a more restrictive set would be better, but trying that yields directories
     *  that won't let users write (don't know about read). Since the directory permissions
     *  will be restricted to the user, I don't think that it's a problem.
     */ 
    private val UserOnlyDirectoryCreatePermissions = EnumSet.allOf( classOf[AclEntryPermission] ).asScala 
    private val UserOnlyFileCreatePermissions      = EnumSet.allOf( classOf[AclEntryPermission] ).asScala // EnumSet.of( READ_DATA, WRITE_DATA, APPEND_DATA, DELETE, READ_ACL ).asScala


    private val UserReadOnlyFileCreatePermissions  = UserOnlyFileCreatePermissions // XXX: DEBUG WHY READ-ACCESS TO AN ACTUALLY READ-ONLY SET FAILS (then remove logged warning)
    //private val UserReadOnlyFileCreatePermissions  = EnumSet.of( READ_DATA, READ_ATTRIBUTES, READ_NAMED_ATTRS, READ_ACL, DELETE ).asScala

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
      builder.setType(AclEntryType.ALLOW)

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
          if ( badPrincipals.size == 0 ) Failable.succeed( path ) else Failable.fail( s"${path} should be a user-only directory, but is accessible by ${badPrincipals.mkString}." )
        }
      }
    }

    def createUserOnlyEmptyFile( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable( Files.createFile( path, fileAttribute( userPrincipal, UserOnlyFileCreatePermissions ) ) )
      }
    }

    // def createUserReadOnlyEmptyFile( path : Path ) : Failable[Path] = {
    //   findJvmUserPrincipal.flatMap { userPrincipal =>
    //     Failable( Files.createFile( path, fileAttribute( userPrincipal, UserReadOnlyFileCreatePermissions ) ) )
    //   }
    // }

    def setUserReadOnlyFilePermissions( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable {
          val view = Files.getFileAttributeView( path, classOf[AclFileAttributeView] )
          val entries = aclEntryList( userPrincipal, UserReadOnlyFileCreatePermissions )
          view.setAcl( entries )

          // XXX: Remove once warned deficiency regarding read-only files is fixed
          INFO.log( s"The file '${path}' could not be made read-only, because doing so would render it inaccessible on Windows. Access is restricted to the current user, however.")

          path
        }
      }
    }

    def setUserOnlyFilePermissions( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable {
          val view = Files.getFileAttributeView( path, classOf[AclFileAttributeView] )
          val entries = aclEntryList( userPrincipal, UserOnlyFileCreatePermissions )
          view.setAcl( entries )
          path
        }
      }
    }

    def setUserOnlyDirectoryPermissions( path : Path ) : Failable[Path] = {
      findJvmUserPrincipal.flatMap { userPrincipal =>
        Failable {
          val view = Files.getFileAttributeView( path, classOf[AclFileAttributeView] )
          val entries = aclEntryList( userPrincipal, UserOnlyDirectoryCreatePermissions )
          view.setAcl( entries )
          path
        }
      }
    }
  }
}
