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
import specification.Types.{ByteSeqExact64, SignatureWithChainIdV, SignatureV, SignatureR, SignatureS, Unsigned256, UnsignedBigInt};

object EthTransaction {
  final object Unsigned {
    private val ZeroElem = RLP.Element.UnsignedInt(0)

    final case class Message( nonce : Unsigned256, gasPrice : Unsigned256, gasLimit : Unsigned256, to : EthAddress, value : Unsigned256, data : immutable.Seq[Byte] )
        extends Unsigned with EthTransaction.Message {
      def sign( signer : EthSigner ) : Signed.NoChainId.Message                 = Signed.NoChainId.Message( this, signer.sign( this.signableBytes( None ) ) );
      def sign( signer : EthSigner, chainId : EthChainId ) : Signed.WithChainId = Signed.WithChainId.Message( this, signer.sign( this.signableBytes( Some( chainId ) ), chainId ) )
    }
    final case class ContractCreation( nonce : Unsigned256, gasPrice : Unsigned256, gasLimit : Unsigned256, value : Unsigned256, init : immutable.Seq[Byte] ) 
        extends Unsigned with EthTransaction.ContractCreation {
      def sign( signer : EthSigner ) : Signed.NoChainId.ContractCreation        = Signed.NoChainId.ContractCreation( this, signer.sign( this.signableBytes( None ) ) );
      def sign( signer : EthSigner, chainId : EthChainId ) : Signed.WithChainId = Signed.WithChainId.ContractCreation( this, signer.sign( this.signableBytes( Some( chainId ) ), chainId ) )
    }
  }
  sealed trait Unsigned extends EthTransaction {

    val unsignedTransaction : EthTransaction.Unsigned = this

    override def signed = false;

    def signableBytes( mbChainId : Option[EthChainId] ) : immutable.Seq[Byte] = {
      mbChainId match {
        case Some( chainId ) => {
          val nosigSeqElement = RLP.toElement[EthTransaction](this)
          assert( nosigSeqElement.isInstanceOf[RLP.Element.Seq], s"We expect transactions to serialize to an RLP Sequence! Instead found ${nosigSeqElement}" )
          val nosigElementSeq = nosigSeqElement.asInstanceOf[RLP.Element.Seq].seq
          val eip155_extraElements =  RLP.Element.UnsignedBigInt(chainId.value.widen) :: Unsigned.ZeroElem :: Unsigned.ZeroElem :: Nil
          val fullSeqElem = RLP.Element.Seq( nosigElementSeq ++ eip155_extraElements )
          RLP.Element.encode( fullSeqElem )
        }
        case None => {
          RLP.encode[EthTransaction]( this )
        }
      }
    }
    def sign( signer : EthSigner ) : Signed.NoChainId;
    def sign( signer : EthSigner, chainId : EthChainId ) : Signed.WithChainId;
    def sign( signer : EthSigner, chainId : Long ) : Signed.WithChainId = sign( signer, EthChainId( chainId ) )
  }
  final object Signed {
    def apply( unsignedTransaction : Unsigned, sig : EthSignature.Base ) : Signed = {
      sig match {
        case simple : EthSignature             => NoChainId  ( unsignedTransaction, simple )
        case wci    : EthSignature.WithChainId => WithChainId( unsignedTransaction, wci    )
      }
    }

    final object NoChainId {
      def apply( unsignedTransaction : Unsigned, sig : EthSignature ) : NoChainId = {
        unsignedTransaction match {
          case msg : Unsigned.Message          => Message( msg, sig );
          case cc  : Unsigned.ContractCreation => ContractCreation( cc, sig );
        }
      }
      final case class Message( unsignedTransaction : Unsigned.Message, val signature : EthSignature ) extends Signed.NoChainId with EthTransaction.Message {
        def nonce    : Unsigned256         = unsignedTransaction.nonce;
        def gasPrice : Unsigned256         = unsignedTransaction.gasPrice;
        def gasLimit : Unsigned256         = unsignedTransaction.gasLimit;
        def to       : EthAddress          = unsignedTransaction.to;
        def value    : Unsigned256         = unsignedTransaction.value;
        def data     : immutable.Seq[Byte] = unsignedTransaction.data;

        override def toString() = s"Signed.NoChainId.Message(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},to=${to},value=${value},data=${data},signature=${signature})"
      }
      final case class ContractCreation( unsignedTransaction : Unsigned.ContractCreation, val signature : EthSignature ) extends Signed.NoChainId with EthTransaction.ContractCreation {
        def nonce    : Unsigned256         = unsignedTransaction.nonce;
        def gasPrice : Unsigned256         = unsignedTransaction.gasPrice;
        def gasLimit : Unsigned256         = unsignedTransaction.gasLimit;
        def value    : Unsigned256         = unsignedTransaction.value;
        def init     : immutable.Seq[Byte] = unsignedTransaction.init;

        override def toString() = s"Signed.NoChainId.ContractCreation(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},,value=${value},init=${init},signature=${signature})"
      }
    }
    sealed trait NoChainId extends Signed {
      val signature : EthSignature
      lazy val signedBytes = unsignedTransaction.signableBytes( None )

      def v : SignatureV = signature.v;
    }

    final object WithChainId {
      def apply( unsignedTransaction : Unsigned, sig : EthSignature.WithChainId ) : WithChainId = {
        unsignedTransaction match {
          case msg : Unsigned.Message          => Message( msg, sig );
          case cc  : Unsigned.ContractCreation => ContractCreation( cc, sig );
        }
      }
      final case class Message( unsignedTransaction : Unsigned.Message, val signature : EthSignature.WithChainId ) extends Signed.WithChainId with EthTransaction.Message {
        def nonce    : Unsigned256         = unsignedTransaction.nonce;
        def gasPrice : Unsigned256         = unsignedTransaction.gasPrice;
        def gasLimit : Unsigned256         = unsignedTransaction.gasLimit;
        def to       : EthAddress          = unsignedTransaction.to;
        def value    : Unsigned256         = unsignedTransaction.value;
        def data     : immutable.Seq[Byte] = unsignedTransaction.data;

        override def toString() = s"Signed.WithChainId.Message(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},to=${to},value=${value},data=${data},signature=${signature})"
      }
      final case class ContractCreation( unsignedTransaction : Unsigned.ContractCreation, val signature : EthSignature.WithChainId ) extends Signed.WithChainId with EthTransaction.ContractCreation {
        def nonce    : Unsigned256         = unsignedTransaction.nonce;
        def gasPrice : Unsigned256         = unsignedTransaction.gasPrice;
        def gasLimit : Unsigned256         = unsignedTransaction.gasLimit;
        def value    : Unsigned256         = unsignedTransaction.value;
        def init     : immutable.Seq[Byte] = unsignedTransaction.init;

        override def toString() = s"Signed.WithChainId.ContractCreation(nonce=${nonce},gasPrice=${gasPrice},gasLimit=${gasLimit},,value=${value},init=${init},signature=${signature})"
      }
    }
    sealed trait WithChainId extends Signed {
      val signature : EthSignature.WithChainId
      lazy val signedBytes = unsignedTransaction.signableBytes( Some( chainId ) )

      def v : SignatureWithChainIdV = signature.v;

      def chainId : EthChainId = signature.chainId
    }
  }
  sealed trait Signed extends EthTransaction{
    val signature           : EthSignature.Base

    def signedBytes : immutable.Seq[Byte]

    def r : SignatureR = signature.r;
    def s : SignatureS = signature.s;

    def untypedV : UnsignedBigInt = signature.untypedV

    lazy val senderPublicKey : EthPublicKey = {
      def fail : EthPublicKey = throw new EthereumException(s"Could not recover public key for signature ${signature} with signed bytes '${signedBytes}'");
      val mbPubKey = signature.wasSigned( signedBytes.toArray )
      mbPubKey.getOrElse( fail )
    }
    lazy val sender : EthAddress = senderPublicKey.toAddress;

    override def signed = true;
  }
  sealed trait Message extends EthTransaction {
    def to   : EthAddress; // not optional once we know we are a message
    def data : immutable.Seq[Byte];

    def isMessage = true;
  }
  sealed trait ContractCreation extends EthTransaction {
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

  def unsignedTransaction : EthTransaction.Unsigned
}


