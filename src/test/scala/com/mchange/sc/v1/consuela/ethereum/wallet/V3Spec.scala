package com.mchange.sc.v1.consuela.ethereum.wallet;

import org.specs2._;

import com.mchange.sc.v1.log.MLogger;
import com.mchange.sc.v1.log.MLevel._;

import play.api.libs.json._

class V3Spec extends Specification {

  private implicit val logger = MLogger( this );

  def is =
s2"""
   A V3 wallet should
       decode to a scrypt-encoded private key consistent with its expected address                              ${e1}
       decode to a pbkdf2-encoded private key consistent with its expected address                              ${e2}
  """

  val jsonScrypt = {
    Json.parse (
      """{"address":"27dd70735fa8cb93c8164b0b43bf733d71638497","crypto":{"cipher":"aes-128-ctr","ciphertext":"c4a499a3b73ebdddaf3526f4b4a996e8012465b0675e66e80f8b2900c9198247","cipherparams":{"iv":"936f111b876c5adba4e4d8b72af13e7d"},"kdf":"scrypt","kdfparams":{"dklen":32,"n":262144,"p":1,"r":8,"salt":"a6fd91afc2fcbf9a61b43894120077090d6330b5e80e6bca9dcce8e5746a08f4"},"mac":"0a600717ee75a350a1d9204ec640df8541d709fe00fd3f6507644ebe69b50896"},"id":"0b1ebaf7-bfdb-4662-9a8e-6c024d024d27","version":3}"""
    )
  }

  val passphraseScrypt = "test"

  val jsonPbkdf2 = Json.parse {
    """{
            "address" : "008aeeda4d805471df9b2a5b0f38a0c3bcba786b",
            "crypto" : {
                "cipher" : "aes-128-ctr",
                "cipherparams" : {
                    "iv" : "6087dab2f9fdbbfaddc31a909735c1e6"
                },
                "ciphertext" : "5318b4d5bcd28de64ee5559e671353e16f075ecae9f99c7a79a38af5f869aa46",
                "kdf" : "pbkdf2",
                "kdfparams" : {
                    "c" : 262144,
                    "dklen" : 32,
                    "prf" : "hmac-sha256",
                    "salt" : "ae3cd4e7013836a3df6bd7241b12db061dbe2c6785853cce422d148a624ce0bd"
                },
                "mac" : "517ead924a9d0dc3124507e3393d175ce3ff7c1e96529c6c555ce9e51205e9b2"
            },
            "id" : "3198bc9c-6672-5ab3-d995-4942343ae5b6",
            "version" : 3
       }"""
  }

  val passphrasePbkdf2 = "testpassword"

  def e1 : Boolean = {
    try {
      V3.decodePrivateKey( V3( jsonScrypt.asInstanceOf[JsObject] ), passphraseScrypt )
      true
    } catch {
      case we : V3.Exception => {
        SEVERE.log("Addresses fail to match, probably, see Exception",we)
        false
      }
    }
  }

  def e2 : Boolean = {
    try {
      V3.decodePrivateKey( V3( jsonPbkdf2.asInstanceOf[JsObject] ), passphrasePbkdf2 )
      true
    } catch {
      case we : V3.Exception => {
        SEVERE.log("Addresses fail to match, probably, see Exception",we)
        false
      }
    }
  }

}
