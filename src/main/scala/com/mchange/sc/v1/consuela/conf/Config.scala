package com.mchange.sc.v1.consuela.conf;

import java.security.Provider;
import java.security.Security;

import com.mchange.sc.v1.consuela.crypto.jce;

import com.typesafe.config.{Config => TSConfig, ConfigFactory => TSConfigFactory};

import com.mchange.sc.v1.log._;
import MLevel._;

object Config {
  implicit val logger = MLogger( this )

  val ConfigName = "consuela";

  private val _inner : TSConfig = TRACE.attempt( TSConfigFactory.load().getConfig( ConfigName ) ).getOrElse( TSConfigFactory.empty("Default settings.") );

  val CryptoJceProviderName               = Item.CryptoJceProviderName.get;                 // Note: For android use "SC"
  val CryptoJceProviderClassNames         = Item.CryptoJceProviderClassNames.get;           // Note: For android include "org.spongycastle.jce.provider.BouncyCastleProvider"
  val CryptoJceForbidUseOfOtherProviders  = Item.CryptoJceForbidUseOfOtherProviders.get;    //       and ensure the spongycastle jce prov lib is bundled, it's not a consuela dependency!

  private[this] object Item {
    val CryptoJceProviderName              = StringItem( "crypto.jce.providerName", "BC" ); //bouncycastle
    val CryptoJceProviderClassNames        = StringListItem( "crypto.jce.providerClassNames", List( "org.bouncycastle.jce.provider.BouncyCastleProvider" ) );
    val CryptoJceForbidUseOfOtherProviders = BooleanItem("crypto.jce.forbidUseOfOtherProviders", false);
  }
  private[this] trait Item[T] {
    def path : String;
    def dflt : T;
    def get  : T;
  }
  private[this] case class StringItem( path : String, dflt : String ) extends Item[String] {
    def get : String = TRACE.attempt( _inner.getString( path ) ).getOrElse( dflt );
  }
  private[this] case class StringListItem( path : String, dflt : List[String] ) extends Item[List[String]] {
    import scala.collection.JavaConverters._;
    def get : List[String] =  TRACE.attempt( _inner.getStringList( path ).asScala.toList ).getOrElse( dflt );
  }
  private[this] case class BooleanItem( path : String, dflt : Boolean ) extends Item[Boolean] {
    def get : Boolean = TRACE.attempt( _inner.getBoolean( path ) ).getOrElse( dflt );
  }
}
