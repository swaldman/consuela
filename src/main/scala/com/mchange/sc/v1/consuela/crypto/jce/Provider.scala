package com.mchange.sc.v1.consuela.crypto.jce;

import com.mchange.sc.v1.consuela.conf.Config._;
import com.mchange.sc.v1.consuela.crypto.ForbiddenProviderException;

import java.security.Security;

import com.mchange.sc.v1.log._;
import MLevel._;

object Provider{
  implicit val logger = MLogger( this );

  val BouncyCastle = Provider("BC");
  val SpongyCastle = Provider("SC");

  val nameToProviderListMap = {
    CryptoJceProviderClassNames
      .map( fqcn => WARNING.attempt( Class.forName( fqcn ).newInstance().asInstanceOf[java.security.Provider] ) )
      .filter( _.isSuccess )
      .map( _.get )
      .groupBy( _.getName() )
  }

  def uniqueProvider( pair : Tuple2[String,List[java.security.Provider]] ) : Boolean = pair._2.length == 1

  assert(
    nameToProviderListMap.forall( uniqueProvider ),
    s"Duplicate names in configured providers: ${nameToProviderListMap.filter( !uniqueProvider( _ ) )}"
  );

  nameToProviderListMap.foreach( pair => WARNING.attempt( Security.addProvider( pair._2.head ) ) );

  val ConfiguredProvider : Provider = {
    assert( nameToProviderListMap.contains( CryptoJceProviderName ), "Failed to load configured JCE provide ${CryptoJceProviderName}." )
    Provider( CryptoJceProviderName );
  }

  def warnForbidUnavailableProvider( source : Any, sourceProvider : Provider )( implicit desiredProvider : Provider ) {
    if ( desiredProvider != sourceProvider ) {
      if ( CryptoJceForbidUseOfOtherProviders ) {
        throw new ForbiddenProviderException( 
          s"'${source}' attempted to use name from JCE provider ${sourceProvider.name} rather than " +
          s"configured provider '${desiredProvider.name}', but such use has been forbidden."
        )
      } else {
        WARNING.log(
          s"Although the configured JCE provider is ${desiredProvider.name}, " +
          s"'${source}' is using APIs from provider ${sourceProvider.name} that are not supported by JCE."
        )
      }
    }
  }
}
final case class Provider( name : String ) // e.g. "BC"
