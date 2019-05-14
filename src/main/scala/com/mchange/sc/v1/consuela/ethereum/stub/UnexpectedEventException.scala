package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent

class UnexpectedEventException( event : SolidityEvent, metadata : Event.Metadata, message : String, cause : Throwable = null ) extends StubException( message, cause )
