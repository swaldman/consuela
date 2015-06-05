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

import specification.Fees.BigInt.G;

import EthWorldState.Account;

object EthTransactionExecutor {

  val Zero = BigInt(0);

  sealed trait ExecutionStatus;

  final object Aborted {
    final object NoAccount extends Aborted;
    final case class MismatchedNonces( acctNonce : Unsigned256, transactionNonce : Unsigned256 ) extends Aborted;
    final case class InsufficientIntrinsicGas( intrinsicGasRequired : Unsigned256, transactionGasLimit : Unsigned256 ) extends Aborted;
    final case class TransactionCostMayExceedAccountBalance( possibleTransactionCost : Unsigned256, accountBalance : Unsigned256 ) extends Aborted;
    final case class TransactionMayExceedBlockGasLimit( transactionGasLimit : Unsigned256, priorTransactionGasUsed : Unsigned256, blockGasLimit : Unsigned256 ) extends Aborted; 
  }
  sealed trait Aborted extends ExecutionStatus;
  object Completed extends ExecutionStatus;

  final case class StateUpdate( finalState : EthWorldState, totalGasUsed : Unsigned256, logEntries : immutable.IndexedSeq[EthLogEntry] );

  private final case class Substate( suicides : Set[EthAddress], logEntries : immutable.IndexedSeq[EthLogEntry], accruedRefund : BigInt );
  private final case class PostExec( provisional : EthWorldState, remainingGas : BigInt, substate : Substate );

  private val OK = Right( () );

  private def execute( 
    worldState : EthWorldState, 
    signedTransaction : EthTransaction.Signed, 
    blockPriorTransactionGasUsed : BigInt, 
    blockGasLimit : BigInt,
    blockCoinbase : EthAddress,
    originalTransactor : Option[EthAddress] = None // set if different from the signer/sender
  ) : (ExecutionStatus, StateUpdate) = {
    def findAccount : Either[ExecutionStatus, Account] = worldState( signedTransaction.sender ).fold( Left[NoAccount] )( Right( _ ) );
    def findGoodNonce( acct : Account ) : Either[ExecutionStatus, BigInt] = {
      if (acct.nonce == signedTransaction.nonce) {
        Right( acct.nonce.widen ) 
      } else {
        Left( MismatchedNonces( acct.nonce, signedTransaction.nonce ) )
      }
    }
    def checkIntrinsicGas : Either[ExecutionStatus, BigInt] = {
      val intrinsicGasRequired = g0( signedTransaction );
      if ( intrinsicGasRequired > signedTransaction.gasLimit.widen ) {
        Left( InsufficientIntrinsicGas( Unsigned256( intrinsicGasRequired ), signedTransaction.gasLimit ) )
      } else {
        intrinsicGasRequired
      }
    }
    def checkPotentialTransactionCost( acct : Account ) : Either[ExecutionStatus, Unit] = { 
      val potentialTransactionCost = v0( signedTransaction );
      if ( potentialTransactionCost > acct.balance ) {
        Left( TransactionCostMayExceedAccountBalance( potentialTransactionCost, acct.balance ) )
      } else {
        OK
      }
    }
    def checkBlockLimit : Either[ExecutionStatus, Unit] = {
      if (blockPriorTransactionGasUsed + signedTransaction.gasLimit > blockGasLimit ) {
        Left( TransactionMayExceedBlockGasLimit( signedTransaction.gasLimit, blockPriorTransactionGasUsed, blockGasLimit ) )
      } else {
        OK
      }
    }
    def acctNonceIntrinsic : Either[ExecutionStatus,Tuple3[Account, BigInt, BigInt]] = {
      for {
        acct                 <- findAccount;
        goodNonce            <- findGoodNince;
        intrinsicGasRequired <- checkIntrinsicGas;
        _ <- checkPotentialTransactionCost( acct );
        _ <- checkBlockLimit
      } yield {
        ( acct, goodNonce, intrinsicGasRequired )
      }
    }
    def checkpointState( acctNonceIntrinsic : (Account, BigInt, BigInt) ) : Either[ExecutionStatus,EthWorldState] = {
      val (acct, oldNonce ) = acctNonce;
      val newNonce   = Unsigned256(acctNonce._2 + 1);
      val newBalance = Unsigned256( signedTransaction.gasLimit.widen * signedTransaction.gasPrice.widen );
      val newAccount = acct.copy( nonce = newNonce, balance = newBalance );
      Right( worldState.including( signedTransaction.sender, newAccount ) )
    }
    def doExec( checkpoint : EthWorldState, intrinsicGasRequired : BigInt ) : Either[ExecutionStatis, PostExec] = { // all other args available from signedTransaction + originalTransactor
      val g = signedTransaction.gasLimit.widen - intrinsicGasRequired;
      ???
    }
    def refundableGas( postExec : PostExec ) : Either[ExecutionState, BigInt] = {
      val remaining      = postExec.remainingGas;
      val extraRefundCap = (signedTransaction.gasLimit - remaining) / 2;
      val extraRefund    = math.min( extraRefundCap, postExec.substate.refundable )
      Right( remaining + extraRefund );
    }
    def preFinalState( provisionalState : EthWorldState, refund : BigInt ) : Either[ExecutionState,EthWorldState] = {
      val provisionalSenderAccount = provisionalState( signedTransaction.sender ).get;
      val newSenderBalance = Unsigned256( provisionalSenderAccount.balance.widen + refund );
      val prefinalSenderAccount = provisionalSenderAccount.copy( balance=newSenderBalance );

      val provisionalCoinbaseAccount = provisionalState( blockCoinbase ).getOrElse( Account.Agent.fresh );
      val newCoinbaseBalance = Unsigned256( provisionalCoinbaseAccount.balance.widen + ( (signedTransaction.gasLimit.widen - refund) * signedTransaction.gasPrice ) );
      val prefinalCoinbaseAccount = provisionalCoinbaseAccount.copy( balance=newCoinbaseBalance );

      Right( provisionalState ++ List( ( signedTransaction.sender, prefinalSenderAccount ), ( blockCoinbase, prefinalCoinbaseAccount ) ) )
    }
    def finalState( prefinalState : EthWorldState, suicides : Set[EthAddress] ) : Either[ExecutionContext, EthWorldState] = {
      Right( prefinalState -- suicides )
    }

  /*
  private def prevalidate( worldState : EthWorldState, signedTransaction : EthTransaction.Signed ) : Boolean = {
    for {
      acct <- worldState( signedTransaction.sender );
      goodNonce <- if (acct.nonce == signedTransaction.nonce) Some( acct.nonce ) else None;

  }

  private def senderKnownWithValidNonce( worldState : EthWorldSate, signedTransaction : EthTransaction.Signed ) : Boolean = {
    worldState( signedTransaction.sender ).fold( false )( acct => acct.nonce == signedTransaction.nonce )
  }
  */ 

  // "intrinsic gas", see yellow paper, sec 6.2
  private def g0( transaction : EthTransaction ) : BigInt  = {
    val bytes = transaction match {
      case msg : EthTransaction.Message          => msg.data;
      case cc  : EthTransaction.ContractCreation => cc.init;
    }
    val variableCost = bytes.map( b => if ( b == 0 ) G.txdatazero else G.txdatanonzero ).foldLeft( Zero )( _ + _ );
    val fixedCost    = G.transaction;

    variableCost + fixedCost
  }

  // "up-front cost", see yellow paper, sec 6.2
  private def v0( txn : EthTransaction ) : BigInt = (txn.gasPrice * txn.gasLimit) + txn.value
}
