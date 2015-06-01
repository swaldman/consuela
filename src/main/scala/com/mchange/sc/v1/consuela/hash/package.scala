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

package com.mchange.sc.v1.consuela;

import java.io.InputStream;

import javax.crypto._;
import java.security._;
import java.security.interfaces._;
import java.security.spec._;

package object hash {

  val DefaultBufferSize = 1024 * 1024;

  def doHash( algoName : String, is : InputStream, bufferSize : Int = DefaultBufferSize )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance( algoName, provider.name );
    val buffer = new Array[Byte]( bufferSize );
    var count = is.read( buffer );
    while ( count >= 0 ) {
      md.update( buffer, 0, count );
      count = is.read( buffer );
    }
    md.digest()
  }
  def doHash( algoName : String, bytes : Array[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = {
    val md = MessageDigest.getInstance(algoName, provider.name );
    md.update( bytes, 0, bytes.length );
    md.digest()
  }
  def doHash( algoName : String, bytes : Seq[Byte] )( implicit provider : crypto.jce.Provider ) : Array[Byte] = doHash( algoName, bytes.toArray )( provider )
}
