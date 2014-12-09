package com.mchange.sc.v1.consuela.conf;

import java.security.Provider;
import java.security.Security;

import com.mchange.sc.v1.consuela.jce;

import com.typesafe.config.{Config => TSConfig, ConfigFactory => TSConfigFactory};

import com.mchange.sc.v1.log._;
import MLevel._;

object Config {
  implicit val logger = MLogger( this )

  val ConfigName = "consuela";

  lazy val _inner : TSConfig = TRACE.attempt( TSConfigFactory.load().getConfig( ConfigName ) ).getOrElse( TSConfigFactory.empty("Default settings.") );

  object Implicits {
    implicit lazy val provider : jce.Provider = jce.Provider( Item.JceProvider.get );
  }

  private[this] object Item {
    val JceProvider           = StringItem( "jce.provider", "BC" ); //bouncycastle
    val JceProviderClassNames = StringListItem( "jce.providerClassNames", List( "org.bouncycastle.jce.provider.BouncyCastleProvider" ) );
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

  // bring in the JCE providers.
  Item.JceProviderClassNames.get.foreach( name => FINER.attempt( Security.addProvider( Class.forName( name ).newInstance().asInstanceOf[Provider] ) ) )
}
