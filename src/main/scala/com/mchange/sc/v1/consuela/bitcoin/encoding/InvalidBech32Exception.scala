package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela.bitcoin.BtcException

class InvalidBech32Exception( message : String, t : Throwable = null ) extends BtcException( message, t )
