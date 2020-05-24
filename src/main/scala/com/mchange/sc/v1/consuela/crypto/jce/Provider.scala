/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.crypto.jce;

import com.mchange.sc.v1.consuela.conf.Config._;
import com.mchange.sc.v1.consuela.crypto.ForbiddenProviderException;

import java.security.Security;

import com.mchange.sc.v1.log.MLevel._;

object Provider{
  implicit lazy val logger = mlogger( this );

  val BouncyCastle = Provider("BC");
  val SpongyCastle = Provider("SC");

  val nameToProviderListMap = {
    CryptoJceProviderClassNames
      .map( fqcn => WARNING.attempt( Class.forName( fqcn ).getDeclaredConstructor().newInstance().asInstanceOf[java.security.Provider] ) )
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
