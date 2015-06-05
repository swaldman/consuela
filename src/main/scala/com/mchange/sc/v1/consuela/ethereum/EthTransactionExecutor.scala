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
import specification.Types.Unsigned256;

import EthWorldState.Account;

import scala.collection._;

object EthTransactionExecutor {

  val Zero    = BigInt(0);
  val Zero256 = Unsigned256(0);

  sealed trait ExecutionStatus;

  final object Aborted {
    final object NoAccount extends Aborted;
    final case class MismatchedNonces( acctNonce : Unsigned256, transactionNonce : Unsigned256 ) extends Aborted;
    final case class InsufficientIntrinsicGas( intrinsicGasRequired : Unsigned256, transactionGasLimit : Unsigned256 ) extends Aborted;
    final case class TransactionCostMayExceedAccountBalance( possibleTransactionCost : Unsigned256, accountBalance : Unsigned256 ) extends Aborted;
    final case class TransactionMayExceedBlockGasLimit( transactionGasLimit : Unsigned256, priorTransactionGasUsed : Unsigned256, blockGasLimit : Unsigned256 ) extends Aborted; 
  }
  sealed trait Aborted extends ExecutionStatus;
  final object Completed extends ExecutionStatus;

  final case class StateUpdate( finalState : EthWorldState, totalGasUsed : Unsigned256, logEntries : immutable.IndexedSeq[EthLogEntry] );

  private final case class Substate( suicides : Set[EthAddress], logEntries : immutable.IndexedSeq[EthLogEntry], accruedRefund : BigInt );
  private final case class PostExec( provisional : EthWorldState, remainingGas : BigInt, substate : Substate );

  private val OK = Right( () );

  private def execute( 
    worldState : EthWorldState, 
    stxn : EthTransaction.Signed, 
    blockPriorTransactionGasUsed : BigInt, 
    blockGasLimit : BigInt,
    blockCoinbase : EthAddress,
    originalTransactor : Option[EthAddress] = None // set if different from the signer/sender
  ) : (ExecutionStatus, StateUpdate) = {
    val sender = stxn.sender;

    def goodAcctNonceIntrinsicGas : Either[ExecutionStatus,Tuple3[Account, BigInt, BigInt]] = {
      def findAccount : Either[ExecutionStatus, Account] = worldState( sender ).fold[Either[ExecutionStatus,Account]]( Left(Aborted.NoAccount) )( Right( _ ) );
      def findGoodNonce( acct : Account ) : Either[ExecutionStatus, BigInt] = {
        if (acct.nonce == stxn.nonce) {
          Right( acct.nonce.widen )
        } else {
          Left( Aborted.MismatchedNonces( acct.nonce, stxn.nonce ) )
        }
      }
      def findGoodIntrinsicGasRequired : Either[ExecutionStatus, BigInt] = {
        val intrinsicGasRequired = g0( stxn );
        if ( intrinsicGasRequired > stxn.gasLimit.widen ) {
          Left( Aborted.InsufficientIntrinsicGas( Unsigned256( intrinsicGasRequired ), stxn.gasLimit ) )
        } else {
          Right( intrinsicGasRequired )
        }
      }
      def checkPotentialTransactionCost( acct : Account ) : Either[ExecutionStatus, Unit] = {
        val potentialTransactionCost = v0( stxn );
        if ( potentialTransactionCost > acct.balance.widen ) {
          Left( Aborted.TransactionCostMayExceedAccountBalance( Unsigned256( potentialTransactionCost ), acct.balance ) )
        } else {
          OK
        }
      }
      def checkBlockLimit : Either[ExecutionStatus, Unit] = {
        if (blockPriorTransactionGasUsed + stxn.gasLimit.widen > blockGasLimit ) {
          Left( Aborted.TransactionMayExceedBlockGasLimit( stxn.gasLimit, Unsigned256( blockPriorTransactionGasUsed ), Unsigned256( blockGasLimit ) ) )
        } else {
          OK
        }
      }

      for {
        acct                 <- findAccount.right;
        goodNonce            <- findGoodNonce( acct ).right;
        intrinsicGasRequired <- findGoodIntrinsicGasRequired.right;
        _ <- checkPotentialTransactionCost( acct ).right;
        _ <- checkBlockLimit.right
      } yield {
        ( acct, goodNonce, intrinsicGasRequired )
      }
    }
    def checkpointState( acct : Account, nonce : BigInt ) : Either[ExecutionStatus,EthWorldState] = {
      val newNonce   = Unsigned256( nonce + 1 );
      val newBalance = Unsigned256( acct.balance.widen - ( stxn.gasLimit.widen * stxn.gasPrice.widen ) );
      val newAccount = acct.copy( nonce = newNonce, balance = newBalance );
      Right( worldState.including( sender, newAccount ) )
    }
    def doExec( checkpoint : EthWorldState, intrinsicGasRequired : BigInt ) : Either[ExecutionStatus, PostExec] = { // all other args available from stxn + originalTransactor
      val g = stxn.gasLimit.widen - intrinsicGasRequired;
      ???
    }
    def refundableGas( postExec : PostExec ) : Either[ExecutionStatus, BigInt] = {
      val remaining      = postExec.remainingGas;
      val extraRefundCap = (stxn.gasLimit.widen - remaining) / 2;
      val extraRefund    = (extraRefundCap) min (postExec.substate.accruedRefund); // odd infix min... oh well
      Right( remaining + extraRefund );
    }
    def preFinalState( provisionalState : EthWorldState, refund : BigInt ) : Either[ExecutionStatus,EthWorldState] = {
      val provisionalSenderAccount = provisionalState( sender ).get;
      val newSenderBalance = Unsigned256( provisionalSenderAccount.balance.widen + refund );
      val prefinalSenderAccount = provisionalSenderAccount.copy( balance=newSenderBalance );

      val provisionalCoinbaseAccount = provisionalState( blockCoinbase ).getOrElse( Account.Agent.fresh );
      val newCoinbaseBalance = Unsigned256( provisionalCoinbaseAccount.balance.widen + ( (stxn.gasLimit.widen - refund) * stxn.gasPrice.widen ) );
      val prefinalCoinbaseAccount = provisionalCoinbaseAccount.copy( balance=newCoinbaseBalance );

      Right( provisionalState ++ List( ( sender, prefinalSenderAccount ), ( blockCoinbase, prefinalCoinbaseAccount ) ) )
    }
    def finalState( prefinalState : EthWorldState, suicides : Set[EthAddress] ) : Either[ExecutionStatus, EthWorldState] = {
      Right( prefinalState -- suicides )
    }

    // expected just a non-nested for comprehension here, with 
    //
    //   ( acct, nonce, intrinsicGasRequired ) <- goodAcctNonceIntrinsicGas.right
    //
    // as its first line.
    //
    // not sure why i had to use this more explicit, convoluted logic.
    //
    val check : Either[ExecutionStatus, StateUpdate] = {
      val ani : Either[ExecutionStatus,Tuple3[Account, BigInt, BigInt]] = goodAcctNonceIntrinsicGas;
      ani match {
        case Left( estatus ) => Left[ExecutionStatus,StateUpdate]( estatus )
        case Right( tup ) => {
          val acct = tup._1;
          val nonce = tup._2;
          val intrinsicGasRequired = tup._3;
          for {
            checkpoint                            <- checkpointState( acct, nonce ).right;
            postExec                              <- doExec( checkpoint, intrinsicGasRequired ).right;
            refund                                <- refundableGas( postExec ).right;
            prefinal                              <- preFinalState( postExec.provisional, refund ).right;
            fin                                   <- finalState( prefinal, postExec.substate.suicides ).right
          } yield {
            StateUpdate( fin, Unsigned256( stxn.gasLimit.widen - postExec.remainingGas ), postExec.substate.logEntries );
          }
        }
      }
    }

    check match {
      case Left( aborted : Aborted ) => ( aborted, StateUpdate( worldState, Zero256, immutable.IndexedSeq.empty[EthLogEntry] ) ); // we remain the original state
      case Left( _ )                 => throw new AssertionError("Eventually we may have execution statuses that provoke partial state changes. But they're not implemented yet.");
      case Right( su : StateUpdate ) => ( Completed, su );
    }
  }

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
  private def v0( txn : EthTransaction ) : BigInt = (txn.gasPrice.widen * txn.gasLimit.widen) + txn.value.widen
}
