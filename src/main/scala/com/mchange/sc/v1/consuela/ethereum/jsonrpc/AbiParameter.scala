package com.mchange.sc.v1.consuela.ethereum.jsonrpc

trait AbiParameter {
  def name         : String
  def `type`       : String
  def internalType : Option[String]
  def tpe          : String = `type`
}
