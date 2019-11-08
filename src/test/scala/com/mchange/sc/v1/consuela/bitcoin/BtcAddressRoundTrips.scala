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

  property("p2shMainnetTextRoundTrips") = forAll { ( payload : ByteSeqExact20 ) =>
    val address = BtcAddress.P2SH_Mainnet.fromPayload( payload.widen )
    BtcAddress( address.text ) == address
  }

  property("p2shMainnetScriptPubKeyRoundTrips") = forAll { ( payload : ByteSeqExact20 ) =>
    val address = BtcAddress.P2SH_Mainnet.fromPayload( payload.widen )
    BtcAddress.recoverFromScriptPubKey( address.toScriptPubKey ).assert == address
  }

  property("p2wpkhMainnetTextRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2WPKH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress( address.text ) == address
  }

  property("p2wpkhMainnetScriptPubKeyRoundTrips") = forAll { ( publicKeyHash : ByteSeqExact20 ) =>
    val address = BtcAddress.P2WPKH_Mainnet.fromPublicKeyHash( publicKeyHash )
    BtcAddress.recoverFromScriptPubKey( address.toScriptPubKey ).assert == address
  }

  property("p2wshMainnetTextRoundTrips") = forAll { ( payload : ByteSeqExact32 ) =>
    val address = BtcAddress.P2WSH_Mainnet.fromPayload( payload.widen )
    BtcAddress( address.text ) == address
  }

  property("p2wshMainnetScriptPubKeyRoundTrips") = forAll { ( payload : ByteSeqExact32 ) =>
    val address = BtcAddress.P2WSH_Mainnet.fromPayload( payload.widen )
    BtcAddress.recoverFromScriptPubKey( address.toScriptPubKey ).assert == address
  }


}
