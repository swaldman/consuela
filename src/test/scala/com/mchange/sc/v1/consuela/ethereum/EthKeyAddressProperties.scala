/*
 * Distributed as part of consuela v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

package com.mchange.sc.v1.consuela.ethereum;

import org.scalacheck.Prop;
import org.scalacheck.Properties;

object EthKeyAddressProperties extends Properties("Ethereum Keys and Addresses") {

  lazy val keyEntropy = new java.security.SecureRandom;

  lazy val priv = EthPrivateKey( keyEntropy );
  lazy val pub  = priv.toPublicKey;
  lazy val addr = pub.toAddress;

  property("EthPrivateKey length is 32 bytes (256 bits)") = Prop( priv.bytes.widen.length == 32 ); // now ensured by types
  property("EthPublicKey length is 64 bytes (512 bits)")  = Prop( pub.bytes.widen.length == 64 );  // now ensured by types
  property("EthAddress length is 20 bytes")               = Prop( addr.bytes.length == 20 ); 

  property("Public Key verifies what the private key signs") = Prop.forAll { (message : Array[Byte]) => pub.verify( message, priv.sign( message ) ) };
}

