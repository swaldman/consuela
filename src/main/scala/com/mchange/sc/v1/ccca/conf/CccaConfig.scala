package com.mchange.sc.v1.ccca.conf;

import com.typesafe.config.{Config => TSConfig, ConfigFactory => TSConfigFactory};

import com.mchange.sc.v1.log._;
import MLevel._;

object Config {
  val ConfigName = "ccca";

  lazy val _inner : TSConfig = TRACE.attempt( Try( TSConfigFactory.load().getConfig( ConfigName ) ) ).getOrElse( TSConfigFactory.empty("Default settings.") );

  object Implicits {
    implicit lazy val provider : jce.Provider = Item.JceProvider.get;
  }

  private[this] object Item {
    val JceProvider           = StringItem( "jce.provider", "BC" ); //bouncycastle
    val JceProviderClassNames = Item[List[String]]( "jce.providerClassNames", List( "org.bouncycastle.jce.provider.BouncyCastleProvider" ) );
  }
  private[this] trait Item[T] {
    def path : String;
    def dflt : T;
    def get  : T;
  }
  private[this] case class StringItem( path : String, dflt : String ) extends Item[String] {
    def get : String = TRACE.attempt( _inner.getString( path ) ).getOrElse( dflt );
  }
  private[this] case class StringListItem( path : String, dflt : List[String] ) extends Item[String] {
    import scala.collection.JavaConverters._;
    def get : List[String] =  TRACE.attempt( _inner.getStringList( path ).asScala ).getOrElse( dflt );
  }
}
