package com.mchange.sc.v1.consuela.ethereum.jsonrpc20

import com.mchange.sc.v1.consuela.ethereum.EthAddress

import scala.collection.immutable
import scala.concurrent.{ExecutionContext,Future}

import java.net.URL

import play.api.libs.json._

object Client {

  final object BlockNumber {
    final case object Earliest extends BlockNumber( JsString( "earliest" ) )
    final case object Latest   extends BlockNumber( JsString( "latest" ) )
    final case object Pending  extends BlockNumber( JsString( "pending" ) )

    final case class Quantity( number : BigInt ) extends BlockNumber( encodeQuantity( number ) )

    def apply( number : BigInt ) : BlockNumber = Quantity( number ) // just create quantity as e.g. BlockNumber(0)
  }
  sealed abstract class BlockNumber( val jsValue : JsValue )


  trait eth {
    def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[immutable.Map[String,Compilation.Contract]]

    def estimateGas(
      from     : Option[EthAddress] = None,
      to       : Option[EthAddress] = None,
      gas      : Option[BigInt]     = None,
      gasPrice : Option[BigInt]     = None,
      value    : Option[BigInt]     = None,
      data     : Option[Seq[Byte]]  = None,
      blockNumber : BlockNumber
    )( implicit ec : ExecutionContext ) : Future[BigInt] 

    def gasPrice()( implicit ec : ExecutionContext )                                                             : Future[BigInt]
    def getCompilers()( implicit ec : ExecutionContext )                                                         : Future[immutable.Set[String]]
    def getTransactionCount( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt]
  }
  class withExchanger( exchanger : Exchanger ) extends Client {
    def extractBigInt( success : Response.Success ) : BigInt = decodeQuantity( success.result.as[String] )

    private def doExchange[T]( methodName : String, params : Seq[JsValue] )( resultBuilder : Response.Success => T )( implicit ec : ExecutionContext ) : Future[T] = {
      exchanger.exchange( methodName, JsArray( params ) ).map( resultBuilder )
    }

    val eth = new Client.eth {
      def compileSolidity( solidityText : String )( implicit ec : ExecutionContext ) : Future[immutable.Map[String,Compilation.Contract]] = {
        doExchange( "eth_compileSolidity", Seq(JsString( solidityText )) )( _.result.as[immutable.Map[String,Compilation.Contract]] )
      }
      def estimateGas(
        from     : Option[EthAddress]          = None,
        to       : Option[EthAddress]          = None,
        gas      : Option[BigInt]              = None,
        gasPrice : Option[BigInt]              = None,
        value    : Option[BigInt]              = None,
        data     : Option[Seq[Byte]] = None,
        blockNumber : BlockNumber
      )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        val params = {
          def listify[T <: JsValue]( key : String, mb : Option[T] ) = mb.fold( Nil : List[Tuple2[String,T]] )( t => List( Tuple2( key, t ) ) )

          val _from     = listify("from", from.map( encodeAddress ))
          val _to       = listify("to", to.map( encodeAddress ))
          val _gas      = listify("gas", gas.map( encodeQuantity ))
          val _gasPrice = listify("gasPrice", gasPrice.map( encodeQuantity ))
          val _value    = listify("value", value.map( encodeQuantity ))
          val _data     = listify("data", data.map( encodeBytes ))
          JsObject( _from ::: _to ::: _gas ::: _gasPrice ::: _value ::: _data ::: Nil )
        }
        doExchange( "eth_estimateGas", Seq(params, blockNumber.jsValue) )( extractBigInt )
      }
      def gasPrice()( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_gasPrice", Seq() )( extractBigInt )
      }
      def getCompilers()( implicit ec : ExecutionContext ) : Future[immutable.Set[String]] = {
        doExchange( "eth_getCompilers", Seq() )( _.result.as[immutable.Set[String]] )
      }
      def getTransactionCount( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_getTransactionCount", Seq( encodeAddress( address ), blockNumber.jsValue ) )( extractBigInt )
      }
    }

    def close() = exchanger.close()
  }
  final object Simple {
    def apply( httpUrl : URL ) = new Client.Simple( httpUrl )
  }
  final class Simple( httpUrl : URL ) extends Client.withExchanger( new Exchanger.Simple( httpUrl ) )
}
trait Client extends AutoCloseable {
  def eth : Client.eth;

  def close()
}