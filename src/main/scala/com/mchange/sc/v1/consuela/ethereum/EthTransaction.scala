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

import com.mchange.sc.v1.consuela._;

import scala.collection._;
import scala.collection.immutable.Vector;
import scala.collection.immutable.Seq;

import java.util.Arrays;
import scala.util.hashing.MurmurHash3;

import encoding.{RLP, RLPSerializing};
import specification.Types.{ByteSeqExact64, SignatureV, SignatureR, SignatureS, Unsigned256};

object EthTransaction {
  final object Unsigned {
    final case class Message( nonce : Unsigned256, gasPrice : Unsigned256, gasLimit : Unsigned256, to : EthAddress, value : Unsigned256, data : immutable.Seq[Byte] ) 
        extends Unsigned with EthTransaction.Message {
      def sign( signer : EthSigner ) : Signed.Message = Signed.Message( this, signer.sign( RLP.encode[EthTransaction]( this ) ) );
    }
    final case class ContractCreation( nonce : Unsigned256, gasPrice : Unsigned256, gasLimit : Unsigned256, value : Unsigned256, init : immutable.Seq[Byte] ) 
        extends Unsigned with EthTransaction.ContractCreation {
      def sign( signer : EthSigner ) : Signed.ContractCreation = Signed.ContractCreation( this, signer.sign( RLP.encode[EthTransaction]( this ) ) );
    }
  }
  sealed trait Unsigned extends EthTransaction {
    override def signed = false;

    def sign( signer : EthSigner ) : Signed;
  }
  final object Signed {
    def apply( base : Unsigned, sig : EthSignature ) : Signed = {
      base match {
        case msg : Unsigned.Message          => Message( msg, sig );
        case cc  : Unsigned.ContractCreation => ContractCreation( cc, sig );
      }
    }
    final case class Message( base : Unsigned.Message, val signature : EthSignature ) extends Signed with EthTransaction.Message {
      def nonce    : Unsigned256         = base.nonce;
      def gasPrice : Unsigned256         = base.gasPrice;
      def gasLimit : Unsigned256         = base.gasLimit;
      def to       : EthAddress          = base.to; 
      def value    : Unsigned256         = base.value;
      def data     : immutable.Seq[Byte] = base.data;

      override def toString() = s"Signed.Message(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},to=${to},value=${value},data=${data},signature=${signature})"
    }
    final case class ContractCreation( base : Unsigned.ContractCreation, val signature : EthSignature ) extends Signed  with EthTransaction.ContractCreation {
      def nonce    : Unsigned256         = base.nonce;
      def gasPrice : Unsigned256         = base.gasPrice;
      def gasLimit : Unsigned256         = base.gasLimit;
      def value    : Unsigned256         = base.value;
      def init     : immutable.Seq[Byte] = base.init;

      override def toString() = s"Signed.ContractCreation(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},,value=${value},init=${init},signature=${signature})"
    }
  }
  sealed trait Signed extends EthTransaction {
    val base      : Unsigned;
    val signature : EthSignature;

    lazy val signedBytes = RLP.encode[EthTransaction](base)

    def v : SignatureV = signature.v;
    def r : SignatureR = signature.r;
    def s : SignatureS = signature.s;

    lazy val senderPublicKey : EthPublicKey = {
      def fail : EthPublicKey = throw new EthereumException(s"Could not recover public key for signature ${signature} with signed bytes '${signedBytes}'");
      val mbPubKey = signature.wasSigned( signedBytes.toArray )
      mbPubKey.getOrElse( fail )
    }
    lazy val sender : EthAddress = senderPublicKey.toAddress;

    override def signed = true;
  }
  trait Message extends EthTransaction {
    def to   : EthAddress; // not optional once we know we are a message
    def data : immutable.Seq[Byte];

    def isMessage = true;
  }
  trait ContractCreation extends EthTransaction {
    def init : immutable.Seq[Byte];

    def isMessage = false;
  }
}
sealed trait EthTransaction {
  def nonce    : Unsigned256;
  def gasPrice : Unsigned256;
  def gasLimit : Unsigned256;
  def value    : Unsigned256;

  def signed   : Boolean;

  def isMessage          : Boolean;
  def isContractCreation : Boolean = !this.isMessage;
}


