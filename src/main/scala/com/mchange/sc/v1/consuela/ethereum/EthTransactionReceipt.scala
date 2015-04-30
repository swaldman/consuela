package com.mchange.sc.v1.consuela.ethereum;

import scala.collection._;
import specification.Types.Unsigned256;
import com.mchange.sc.v1.consuela.bloom.BitSetBloom;

object EthTransactionReceipt {
  def apply( postTransactionState : EthHash, gasUsed : Unsigned256, logEntries : immutable.Seq[EthLogEntry] ) : EthTransactionReceipt = {
    this.apply( postTransactionState, gasUsed, computeLogsBloom( logEntries ), logEntries );
  }
  def computeLogsBloom( entries : Seq[EthLogEntry] ) : EthLogBloom = BitSetBloom[EthLogEntry]( entries : _* )
}
case class EthTransactionReceipt( postTransactionState : EthHash, gasUsed : Unsigned256, logsBloom : EthLogBloom, logEntries : immutable.Seq[EthLogEntry] ) {
  lazy val isValid : Boolean = ( logsBloom == EthTransactionReceipt.computeLogsBloom( logEntries ) );

  def isInvalid : Boolean = !this.isValid;
}
