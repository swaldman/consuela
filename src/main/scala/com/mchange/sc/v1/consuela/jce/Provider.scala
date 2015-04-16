package com.mchange.sc.v1.consuela.jce;

import com.mchange.sc.v1.consuela.conf.Config._;

import java.security.Security;

import com.mchange.sc.v1.log._;
import MLevel._;

object Provider{
  implicit val logger = MLogger( this );

  val ConfiguredProvider : Provider = {
    val forbid = JceForbidOtherCrypto;
    val configuredProvider = Provider( JceProvider );
    if (configuredProvider.code != BouncyCastleProviderCode) {
      if (forbid) {
        WARNING.log(
          s"You have configured a JCE provider ('${configuredProvider.code}') other then consuela's default bouncycastle provider, and configured " +
            "to forbid use of crypto code from other than the provider you have selected. consuela makes some (minimal) use of bouncycastle crypto " +
            "not accessible through the JCE apis. Attempts to use this code will now fail with a ForbiddenProviderException."
        )
      } else {
        WARNING.log(
          s"Will attempt to use JCE provider ${configuredProvider.code} where possible. " +
            s"But please not that consuela (unfortunately) makes some use of non-JCE-available APIs of its default provider bouncycastle (${BouncyCastleProviderCode}). " +
            "More warnings will be emitted when such code is used."
        );
      }
    }
    JceProviderClassNames.foreach { name =>
      val newProvider = Class.forName( name ).newInstance().asInstanceOf[java.security.Provider];
      if ( configuredProvider.code == BouncyCastleProviderCode &&
        newProvider.getName() == BouncyCastleProviderCode &&
        newProvider.getClass().getName() != BouncyCastleProviderFqcn ) {
        WARNING.log(
          s"A JCE Provider tried to register with default provider name ${BouncyCastleProviderCode} but with unexpected class ${newProvider.getClass().getName()} " +
            " Not loading this provider, skipping..."
        )
      } else {
        FINER.attempt( Security.addProvider( newProvider ) )
      }
    }
    configuredProvider
  }

  def warnForbidUnconfiguredUseOfBouncyCastle( source : AnyRef )( implicit desiredProvider : Provider ) {
    if ( desiredProvider.code != BouncyCastleProviderCode ) {
      if ( JceForbidOtherCrypto ) {
        throw new ForbiddenProviderException(
          s"'${source}' attempted to use code from default JCE provider bouncycastle rather than " +
           "configured provider '${desiredProvider.code}', but such use has been forbidden."
        )
      } else {
        WARNING.log(
          s"Although the configured JCE provider is ${desiredProvider.code}, '${source}' is using APIs of the bouncycastle crypto library not supported by JCE. "
        )
      }
    }
  }
}
case class Provider( code : String )
