package com.mchange.sc.v1.consuela.ethereum.jsonrpc20.playws

import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.{Client => GenericClient}

class Client( exchanger : Exchanger ) extends GenericClient.withExchanger( exchanger );

