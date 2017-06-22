package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthTransaction}
import com.mchange.sc.v1.consuela.ethereum.encoding.RLP

import com.mchange.sc.v2.jsonrpc._
import jetty.JettyExchanger

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

import java.net.URL

import play.api.libs.json._

import com.mchange.sc.v1.log.MLevel._

import com.mchange.sc.v2.yinyang._

object Client {
  implicit lazy val logger = mlogger( this )

  final object BlockNumber {
    final case object Earliest extends BlockNumber( JsString( "earliest" ) )
    final case object Latest   extends BlockNumber( JsString( "latest" ) )
    final case object Pending  extends BlockNumber( JsString( "pending" ) )

    final case class Quantity( number : BigInt ) extends BlockNumber( encodeQuantity( number ) )

    def apply( number : BigInt ) : BlockNumber = Quantity( number ) // just create quantity as e.g. BlockNumber(0)
  }
  sealed abstract class BlockNumber( val jsValue : JsValue )


  trait eth {
    def call(
      from        : Option[EthAddress]     = None,
      to          : Option[EthAddress]     = None,
      gas         : Option[BigInt]         = None,
      gasPrice    : Option[BigInt]         = None,
      value       : Option[BigInt]         = None,
      data        : Option[Seq[Byte]]      = None,
      blockNumber : BlockNumber            = BlockNumber.Latest
    )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Byte]]

    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[immutable.Map[String,Compilation.Contract]]

    def estimateGas(
      from     : Option[EthAddress] = None,
      to       : Option[EthAddress] = None,
      gas      : Option[BigInt]     = None,
      gasPrice : Option[BigInt]     = None,
      value    : Option[BigInt]     = None,
      data     : Option[Seq[Byte]]  = None
    )( implicit ec : ExecutionContext ) : Future[BigInt] 

    def gasPrice()( implicit ec : ExecutionContext )                                                             : Future[BigInt]
    def getBalance( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext )          : Future[BigInt]
    def getCode( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext )             : Future[immutable.Seq[Byte]]
    def getCompilers()( implicit ec : ExecutionContext )                                                         : Future[immutable.Set[String]]
    def getTransactionCount( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt]
    def getTransactionReceipt( transactionHash : EthHash )( implicit ec : ExecutionContext )                     : Future[Option[ClientTransactionReceipt]]

    def sendRawTransaction( bytes : Seq[Byte] )( implicit ec : ExecutionContext ) : Future[EthHash]

    def sendSignedTransaction( signedTransaction : EthTransaction.Signed )( implicit ec : ExecutionContext ) : Future[EthHash]
  }

  class forExchanger( exchanger : Exchanger ) extends Client {
    def extractBigInt( success : Response.Success ) : BigInt = decodeQuantity( success.result.as[String] )

    def responseHandler[T]( onSuccess : Response.Success => T ) : Response => T = { result =>
      result match {
        case Yang( success ) => onSuccess( success )
        case Yin( error )    => error.vomit
      }
    }

    private def doExchange[T]( methodName : String, params : Seq[JsValue] )( successHandler : Response.Success => T )( implicit ec : ExecutionContext ) : Future[T] = {
      TRACE.log( s"methodName = ${methodName}; params = ${params}" )
      exchanger.exchange( methodName, JsArray( params ) ).map( responseHandler( successHandler ) )
    }

    val eth = new Client.eth {

      private def createTransactionCallObject(
        from     : Option[EthAddress] = None,
        to       : Option[EthAddress] = None,
        gas      : Option[BigInt]     = None,
        gasPrice : Option[BigInt]     = None,
        value    : Option[BigInt]     = None,
        data     : Option[Seq[Byte]]  = None
      ) : JsObject = {
        def listify[T <: JsValue]( key : String, mb : Option[T] ) = mb.fold( Nil : List[Tuple2[String,T]] )( t => List( Tuple2( key, t ) ) )
        val _from     = listify("from", from.map( encodeAddress ))
        val _to       = listify("to", to.map( encodeAddress ))
        val _gas      = listify("gas", gas.map( encodeQuantity ))
        val _gasPrice = listify("gasPrice", gasPrice.map( encodeQuantity ))
        val _value    = listify("value", value.map( encodeQuantity ))
        val _data     = listify("data", data.map( encodeBytes ))
        JsObject( _from ::: _to ::: _gas ::: _gasPrice ::: _value ::: _data ::: Nil )
      }

      def call(
        from        : Option[EthAddress]     = None,
        to          : Option[EthAddress]     = None,
        gas         : Option[BigInt]         = None,
        gasPrice    : Option[BigInt]         = None,
        value       : Option[BigInt]         = None,
        data        : Option[Seq[Byte]]      = None,
        blockNumber : BlockNumber            = BlockNumber.Latest
      )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Byte]] = {
        val txnCallObject = createTransactionCallObject( from, to, gas, gasPrice, value, data )
        doExchange( "eth_call", Seq(txnCallObject, blockNumber.jsValue) )( success => decodeBytes( success.result.as[String] ) )
      }
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[immutable.Map[String,Compilation.Contract]] = {
        doExchange( "eth_compileSolidity", Seq(JsString( solidityText )) )( _.result.as[immutable.Map[String,Compilation.Contract]] )
      }
      def estimateGas(
        from     : Option[EthAddress] = None,
        to       : Option[EthAddress] = None,
        gas      : Option[BigInt]     = None,
        gasPrice : Option[BigInt]     = None,
        value    : Option[BigInt]     = None,
        data     : Option[Seq[Byte]]  = None
      )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        val txnCallObject = createTransactionCallObject( from, to, gas, gasPrice, value, data )
        doExchange( "eth_estimateGas", Seq(txnCallObject) )( extractBigInt )
      }
      def gasPrice()( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_gasPrice", Seq() )( extractBigInt )
      }
      def getBalance( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_getBalance", Seq( encodeAddress( address ), blockNumber.jsValue ) )( extractBigInt )
      }
      def getCode( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Byte]] = {
        doExchange( "eth_getCode", Seq( encodeAddress( address ), blockNumber.jsValue ) )( resp => decodeBytes( resp.result.as[String] ) )
      }
      def getCompilers()( implicit ec : ExecutionContext ) : Future[immutable.Set[String]] = {
        doExchange( "eth_getCompilers", Seq() )( _.result.as[immutable.Set[String]] )
      }
      def getTransactionReceipt( transactionHash : EthHash )( implicit ec : ExecutionContext ) : Future[Option[ClientTransactionReceipt]] = {
        doExchange( "eth_getTransactionReceipt", Seq( encodeBytes( transactionHash.bytes ) ) )( _.result.as[Option[ClientTransactionReceipt]] )
      }
      def sendRawTransaction( bytes : Seq[Byte] )( implicit ec : ExecutionContext ) : Future[EthHash] = {
        doExchange( "eth_sendRawTransaction", Seq( encodeBytes( bytes ) ) )( success => EthHash.withBytes( decodeBytes( success.result.as[String] ) ) )
      }
      def sendSignedTransaction( signedTransaction : EthTransaction.Signed )( implicit ec : ExecutionContext ) : Future[EthHash] = {
        TRACE.log( s"sendSignedTransaction: ${signedTransaction}" )
        TRACE.log( s"recovered sender address: ${signedTransaction.sender}" )
        sendRawTransaction( RLP.encode( signedTransaction : EthTransaction ) )
      }
      def getTransactionCount( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_getTransactionCount", Seq( encodeAddress( address ), blockNumber.jsValue ) )( extractBigInt )
      }
    }

    def close() = exchanger.close()
  }

  final object Factory {
    def createSimpl() : Factory = Client.Simple
    def createAsync() : Factory = new Async

    class Async extends Client.Factory {
      val exfactory = new JettyExchanger.Factory

      def apply( httpUrl : URL ) : Client = new Client.forExchanger( exfactory( httpUrl ) )

      def close() : Unit = exfactory.close()
    }
  }
  trait Factory extends AutoCloseable {
    def apply( httpUrl : URL ) : Client
    def apply( httpUrl : String ) : Client = this.apply( new URL( httpUrl ) )

    def close() : Unit
  }
  final object Simple extends Factory{
    // annoyingly, only one of these is permit to have a default ExecutionContext
    def apply( httpUrl : URL ) = new Client.Simple( httpUrl )
    def close() : Unit = ()
  }
  final class Simple( httpUrl : URL ) extends Client.forExchanger( new Exchanger.Simple( httpUrl ) )
}
trait Client extends AutoCloseable {
  def eth : Client.eth;

  def close()
}
