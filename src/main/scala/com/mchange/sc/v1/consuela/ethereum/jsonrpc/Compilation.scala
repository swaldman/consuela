package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import play.api.libs.json._
import scala.collection._

/**
  * A Compilation is just a mapping of String -> Compilation.Contract. We define this as a type
  * definition, in the package object (package.scala), rather than as a separate file, because Scala does not
  * permit top-level type declarations, they must be in a class, trait, or object (but package
  * objects are fine.
  */ 
object Compilation {

  def opt( str : String ) : Option[String] = if( str.trim == "" ) None else Some( str )

  def str[T : Format]( t : T ) : String = Json.stringify( Json.toJson( t ) )

  def optStr[T <: MaybeEmpty : Format ]( t : T ) : Option[String] = if ( t.isEmpty ) None else Some( str( t ) )

  def opt[T <: MaybeEmpty : Format ]( t : T ) : Option[T] = if ( t.isEmpty ) None else Some( t )

  final object Doc {
    final object User {
      final case class MethodInfo( notice : Option[String] )
    }
    final case class User( methods : Option[immutable.Map[String,User.MethodInfo]] ) extends MaybeEmpty {
      def isEmpty : Boolean = methods.isEmpty || methods.get.isEmpty
    }

    final object Developer {
      final case class MethodInfo( details : Option[String], params : Option[immutable.Map[String,String]] )
    }
    final case class Developer( title : Option[String], methods : Option[immutable.Map[String, Developer.MethodInfo]] ) extends MaybeEmpty {
      def isEmpty : Boolean = title.isEmpty && ( methods.isEmpty || methods.get.isEmpty )
    }
  }

  final object Contract {

    // most fields always have some non-null value, but often it is something uselessly empty
    //
    // metadata is only just proposed to be defined, and in any event would only be available
    // for outputs from very recent compiler versions
    //
    // use the mbXXX methods to get a useful Option (for which the empty values get represented as None)

    final case class Info (
      source          : Option[String],
      language        : Option[String],
      languageVersion : Option[String],
      compilerVersion : Option[String],
      compilerOptions : Option[String],
      abi             : Option[Abi],
      userDoc         : Option[Doc.User],
      developerDoc    : Option[Doc.Developer],
      metadata        : Option[String],
      ast             : Option[String],
      sourceTimestamp : Option[Long]
    ) {
      def mbSource               = source.flatMap( opt )
      def mbLanguage             = language.flatMap( opt )
      def mbLanguageVersion      = languageVersion.flatMap( opt )
      def mbCompilerVersion      = compilerVersion.flatMap( opt )
      def mbCompilerOptions      = compilerOptions.flatMap( opt )
      def mbAbi                  = abi.flatMap( opt[Abi] )
      def mbAbiAsString          = abi.flatMap( optStr[Abi] )
      def mbUserDoc              = userDoc.flatMap( opt[Doc.User] )
      def mbUserDocAsString      = userDoc.flatMap( optStr[Doc.User] )
      def mbDeveloperDoc         = developerDoc.flatMap( opt[Doc.Developer] )
      def mbDeveloperDocAsString = developerDoc.flatMap( optStr[Doc.Developer] )
      def mbMetadata             = metadata.flatMap( opt )
      def mbAst                  = ast.flatMap( opt )
      def mbSourceTimestamp      = sourceTimestamp
    }
  }
  final case class Contract( code : String, info : Contract.Info )
}
// we have to leave this in the package object,
// since type declarations can't be top level
// type Compilation = immutable.Map[String,Compilation.Contract]
