package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela.bitcoin.BtcException

class InvalidSegWitException( message : String, t : Throwable = null ) extends BtcException( message, t )
