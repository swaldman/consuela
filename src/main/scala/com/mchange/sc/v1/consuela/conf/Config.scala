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

package com.mchange.sc.v1.consuela.conf;

import java.security.Provider;
import java.security.Security;

import com.mchange.sc.v1.consuela.crypto.jce;
import com.mchange.sc.v1.consuela.ethereum.pow.ethash23;

import com.typesafe.config.{Config => TSConfig, ConfigFactory => TSConfigFactory};

import com.mchange.sc.v1.log.MLevel._;

object Config {
  implicit lazy val logger = mlogger( this )

  val ConfigName = "consuela";

  private val _inner : TSConfig = TRACE.attempt( TSConfigFactory.load().getConfig( ConfigName ) ).getOrElse( TSConfigFactory.empty("Default settings.") );

  val CryptoJceProviderName               = Item.CryptoJceProviderName.get;                 // Note: For android use "SC"
  val CryptoJceProviderClassNames         = Item.CryptoJceProviderClassNames.get;           // Note: For android include "org.spongycastle.jce.provider.BouncyCastleProvider"
  val CryptoJceForbidUseOfOtherProviders  = Item.CryptoJceForbidUseOfOtherProviders.get;    //       and ensure the spongycastle jce prov lib is bundled, it's not a consuela dependency!

  val EthereumPowFull = Item.EthereumPowFull.get;

  val EthereumPowEthash23SeedPrimerEpochNumber = Item.EthereumPowEthash23SeedPrimerEpochNumber.get;
  val EthereumPowEthash23SeedPrimerValue       = Item.EthereumPowEthash23SeedPrimerValue.get;
  val EthereumPowEthash23DagFileDirectory      = Item.EthereumPowEthash23DagFileDirectory.get;
  val EthereumPowEthash23ManagerDoubleDag      = Item.EthereumPowEthash23ManagerDoubleDag.get;
  val EthereumPowEthash23DagInMemGenFactor     = Item.EthereumPowEthash23DagInMemGenFactor.get;

  private[this] final object Item {
    val CryptoJceProviderName              = StringItem( "crypto.jce.providerName", "BC" ); //bouncycastle
    val CryptoJceProviderClassNames        = StringListItem( "crypto.jce.providerClassNames", List( "org.bouncycastle.jce.provider.BouncyCastleProvider" ) );
    val CryptoJceForbidUseOfOtherProviders = BooleanItem("crypto.jce.forbidUseOfOtherProviders", false);

    val EthereumPowFull                          = BooleanItem( "ethereum.pow.full", false );
    val EthereumPowEthash23SeedPrimerEpochNumber = LongItem( "ethereum.pow.ethash23.seed.primer.epochNumber", 0L );
    val EthereumPowEthash23SeedPrimerValue       = StringItem( "ethereum.pow.ethash23.seed.primer.value", "0x0000000000000000000000000000000000000000000000000000000000000000" );
    val EthereumPowEthash23DagFileDirectory      = StringItem( "ethereum.pow.ethash23.dagfile.directory",  ethash23.DagFile.DefaultDirectory );
    val EthereumPowEthash23ManagerDoubleDag      = BooleanItem( "ethereum.pow.ethash23.manager.doubleDag",  false );
    val EthereumPowEthash23DagInMemGenFactor     = DoubleItem( "ethereum.pow.ethash23.dag.inMemGenFactor",  2.5d );
  }
  private[this] trait Item[T] {
    def path : String;
    def dflt : T;
    def get  : T;
  }
  private[this] final case class StringItem( path : String, dflt : String ) extends Item[String] {
    def get : String = TRACE.attempt( _inner.getString( path ) ).getOrElse( dflt );
  }
  private[this] final case class StringListItem( path : String, dflt : List[String] ) extends Item[List[String]] {
    import scala.collection.JavaConverters._;
    def get : List[String] =  TRACE.attempt( _inner.getStringList( path ).asScala.toList ).getOrElse( dflt );
  }
  private[this] final case class BooleanItem( path : String, dflt : Boolean ) extends Item[Boolean] {
    def get : Boolean = TRACE.attempt( _inner.getBoolean( path ) ).getOrElse( dflt );
  }
  private[this] final case class LongItem( path : String, dflt : Long ) extends Item[Long] {
    def get : Long = TRACE.attempt( _inner.getLong( path ) ).getOrElse( dflt );
  }
  private[this] final case class DoubleItem( path : String, dflt : Double ) extends Item[Double] {
    def get : Double = TRACE.attempt( _inner.getDouble( path ) ).getOrElse( dflt );
  }
}
