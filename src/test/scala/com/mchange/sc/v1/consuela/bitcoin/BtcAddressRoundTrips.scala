package com.mchange.sc.v1.consuela.bitcoin

import com.mchange.sc.v1.consuela._

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

import com.mchange.sc.v1.consuela.ethereum.specification.Arbitraries._

object BtcAddressRoundtrips extends Properties("BtcAddressRoundtrips") {

  property("p2pkhMainnetTextRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2PKH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress( address.text ) == address
  }

  property("p2pkhMainnetScriptPubKeyRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2PKH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress.recoverFromScriptPubKey( address.toScriptPubKey ).assert == address
  }

  property("p2shMainnetTextRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2SH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress( address.text ) == address
  }

  property("p2shMainnetScriptPubKeyRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2SH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress.recoverFromScriptPubKey( address.toScriptPubKey ).assert == address
  }

}
