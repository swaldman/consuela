package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthLogEntry,EthTransaction}
import com.mchange.sc.v1.consuela.ethereum.encoding.RLP
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}

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

  sealed trait Filter {
    def identifier : String
  }
  final class BlockFilter( val identifier : String ) extends Filter

  final object LogFilter {
    object TopicRestriction {
      final case class  Exact( topic : EthLogEntry.Topic   ) extends TopicRestriction {
        val jsValue = encodeBytes( topic.widen )
      }
      final case class  AnyOf( topics : EthLogEntry.Topic* ) extends TopicRestriction {
        val jsValue = JsArray( topics.map( topic => encodeBytes( topic.widen ) ) )
      }
      final case object Any extends TopicRestriction {
        val jsValue = JsNull
      }
    }
    sealed trait TopicRestriction {
      def jsValue : JsValue
    }
    final case class Query(
      address       : Option[EthAddress],
      fromBlock     : BlockNumber = BlockNumber.Latest,
      toBlock       : BlockNumber = BlockNumber.Latest,
      restriction_1 : TopicRestriction = TopicRestriction.Any,
      restriction_2 : TopicRestriction = TopicRestriction.Any,
      restriction_3 : TopicRestriction = TopicRestriction.Any,
      restriction_4 : TopicRestriction = TopicRestriction.Any
    ) {
      def jsValue = {
        val fields = {
          val always = immutable.Seq(
            "fromBlock" -> fromBlock.jsValue,
            "toBlock"   -> toBlock.jsValue,
            "topics"    -> JsArray( restriction_1.jsValue :: restriction_2.jsValue :: restriction_3.jsValue :: restriction_4.jsValue :: Nil )
          )
          address.fold( always )( a => always :+ ("address" -> encodeBytes( a.bytes.widen )) )
        }
        JsObject( fields )
      }
    }
  }
  final class LogFilter( val identifier : String ) extends Filter

  private object RawLog {
    implicit val RawLogFormat = Json.format[RawLog]
  }
  private final case class RawLog(
    removed          : Boolean,
    logIndex         : Option[Unsigned256],
    transactionIndex : Option[Unsigned256],
    transactionHash  : Option[EthHash],
    blockHash        : Option[EthHash],
    blockNumber      : Option[Unsigned256],
    address          : EthAddress,
    data             : immutable.Seq[Byte],
    topics           : immutable.IndexedSeq[ByteSeqExact32]
  )

  object Log {
    def apply( rl : RawLog ) : Log = {
      val optionals = immutable.Seq.apply[Option[Any]]( rl.logIndex, rl.transactionIndex, rl.transactionHash, rl.blockHash, rl.blockNumber )
      val looksPending  = optionals.forall( _.isEmpty )
      val looksRecorded = optionals.forall( _.nonEmpty )

      ( looksPending, looksRecorded, rl.removed ) match {
        case ( true, false, false ) => Pending( rl.address, rl.data, rl.topics )
        case ( true, false, true ) => Removed( rl.address, rl.data, rl.topics )
        case ( false, true, false ) => {
          Recorded(
            logIndex = rl.logIndex.get,
            transactionIndex = rl.transactionIndex.get,
            transactionHash = rl.transactionHash.get,
            blockHash = rl.blockHash.get,
            blockNumber = rl.blockNumber.get,
            address = rl.address,
            data = rl.data,
            topics = rl.topics
          )
        }
        case ( false, true, true ) => {
          WARNING.log( s"${rl} is a removed log entry from a block with full block information. Perhaps we should redefine (or bifurcate) Client.Log.Pending" )
          Pending( rl.address, rl.data, rl.topics )
        }
        case ( true, true, _ ) | ( false, false, _ ) => throw new InternalError( s"${optionals} can't both be nonEmpty and empty at the same time. Should never happen." )
      }
    }
    final case class Pending (
      val address : EthAddress,
      val data    : immutable.Seq[Byte],
      val topics  : immutable.IndexedSeq[ByteSeqExact32]
    ) extends Log

    final case class Removed (
      val address : EthAddress,
      val data    : immutable.Seq[Byte],
      val topics  : immutable.IndexedSeq[ByteSeqExact32]
    ) extends Log

    final case class Recorded (
      logIndex         : Unsigned256,
      transactionIndex : Unsigned256,
      transactionHash  : EthHash,
      blockHash        : EthHash,
      blockNumber      : Unsigned256,
      address          : EthAddress,
      data             : immutable.Seq[Byte],
      topics           : immutable.IndexedSeq[ByteSeqExact32]
    ) extends Log
  }
  sealed trait Log {
    def address : EthAddress
    def data    : immutable.Seq[Byte]
    def topics  : immutable.IndexedSeq[ByteSeqExact32]
  }



  val EmptyParams = Seq.empty[JsValue]

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
    def getBlockFilterChanges( filter : BlockFilter )( implicit ec : ExecutionContext )                          : Future[immutable.Seq[EthHash]]
    def newBlockFilter()( implicit ec : ExecutionContext )                                                       : Future[BlockFilter]
    def newLogFilter( query : LogFilter.Query )( implicit ec : ExecutionContext )                                : Future[LogFilter]
    def sendRawTransaction( bytes : Seq[Byte] )( implicit ec : ExecutionContext )                                : Future[EthHash]
    def sendSignedTransaction( signedTransaction : EthTransaction.Signed )( implicit ec : ExecutionContext )     : Future[EthHash]
    def uninstallFilter( filter : Filter )( implicit ec : ExecutionContext )                                     : Future[Boolean]
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

    final object ExchangerEth extends Client.eth {

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
        doExchange( "eth_gasPrice", EmptyParams )( extractBigInt )
      }
      def getBalance( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt] = {
        doExchange( "eth_getBalance", Seq( encodeAddress( address ), blockNumber.jsValue ) )( extractBigInt )
      }
      def getCode( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Byte]] = {
        doExchange( "eth_getCode", Seq( encodeAddress( address ), blockNumber.jsValue ) )( resp => decodeBytes( resp.result.as[String] ) )
      }
      def getCompilers()( implicit ec : ExecutionContext ) : Future[immutable.Set[String]] = {
        doExchange( "eth_getCompilers", EmptyParams )( _.result.as[immutable.Set[String]] )
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
      def newBlockFilter()( implicit ec : ExecutionContext ) : Future[BlockFilter] = {
        doExchange( "eth_newBlockFilter", EmptyParams )( success => new BlockFilter( success.result.as[String] ) )
      }
      def newLogFilter( query : LogFilter.Query )( implicit ec : ExecutionContext ) : Future[LogFilter] = {
        doExchange( "eth_newFilter", Seq( query.jsValue ) )( success => new LogFilter( success.result.as[String] ) )
      }
      def getBlockFilterChanges( filter : BlockFilter )( implicit ec : ExecutionContext ) : Future[immutable.IndexedSeq[EthHash]] = {
        doExchange( "eth_getFilterChanges", Seq( JsString(filter.identifier) ) )( success =>  success.result.as[immutable.IndexedSeq[JsValue]].map( jss => EthHash.withBytes( jss.as[String].decodeHex ) ) )
      }
      def uninstallFilter( filter : Filter )( implicit ec : ExecutionContext ) : Future[Boolean] = {
        doExchange( "eth_uinstallFilter", Seq( JsString( filter.identifier ) ) )( success =>  success.result.as[Boolean] )
      }
    }

    val eth = ExchangerEth

    def close() = exchanger.close()
  }

  final object Factory {
    def createSimpleFactory() : Factory       = Client.Simple
    def createAsyncFactory()  : Factory.Async = new Async( new JettyExchanger.Factory )

    implicit lazy val Default = new Async( Exchanger.Factory.Default ) // wrap around the default Exchanger, which is asynchronous

    class Async( exchangerFactory : Exchanger.Factory.Async ) extends Client.Factory {

      def apply( httpUrl : URL ) : Client = new Client.forExchanger( exchangerFactory( httpUrl ) )

      def close() : Unit = exchangerFactory.close()
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
