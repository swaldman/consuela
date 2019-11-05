package com.mchange.sc.v1.consuela.bitcoin.encoding

import com.mchange.sc.v1.consuela.bitcoin.BtcException

class IllegalLetterException( message : String, t : Throwable = null ) extends BtcException( message, t )
