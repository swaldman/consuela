package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela.bitcoin.BtcException

class Bech32ChecksumFailedException( message : String, t : Throwable = null ) extends InvalidBech32Exception( message, t )
