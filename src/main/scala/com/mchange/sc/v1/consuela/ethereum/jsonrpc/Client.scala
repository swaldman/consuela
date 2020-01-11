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

  object Filter {
    final case object Dummy extends Filter {
      def identifier : String = null
    }
  }
  sealed trait Filter {
    def identifier : String
  }
  final class BlockFilter( val identifier : String ) extends Filter

  private object RawLog {
    implicit val RawLogFormat = Json.format[RawLog]
  }
  private final case class RawLog(
    removed          : Option[Boolean],
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
    final object Filter {
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
        addresses     : Seq[EthAddress]     = Nil,
        fromBlock     : Option[BlockNumber] = None,
        toBlock       : Option[BlockNumber] = None,
        restriction_1 : TopicRestriction    = TopicRestriction.Any,
        restriction_2 : TopicRestriction    = TopicRestriction.Any,
        restriction_3 : TopicRestriction    = TopicRestriction.Any,
        restriction_4 : TopicRestriction    = TopicRestriction.Any,
        minTopics     : Int                 = 0
      ) {
        require( minTopics <= 4, s"The maximum number of topics an event supports is 4. minTopics cannot be ${minTopics}" )
        lazy val topicRestrictionList = {
          val rawReverse = restriction_4 :: restriction_3 :: restriction_2 :: restriction_1 :: Nil

          // JsNulls mean any topic, but insist on existence. By chopping the tail, we support fewer than 4 topics present
          val rawReverseClipped = rawReverse.dropWhile( _ == TopicRestriction.Any )

          def fulfillMinimumReverse( lastReverse : List[TopicRestriction] ) : List[TopicRestriction] = {
            if (lastReverse.length < minTopics ) {
              fulfillMinimumReverse( TopicRestriction.Any :: lastReverse )
            }
            else {
              lastReverse
            }
          }

          val finalReverse = fulfillMinimumReverse( rawReverseClipped )
          finalReverse.reverse
        }
        def jsValue = {
          val fields = {
            val f1 : List[Tuple2[String,JsValue]] = {
              addresses.length match {
                case 0 => Nil
                case 1 => ( "address" -> encodeAddress( addresses(0) ) ) :: Nil
                case _ => ( "address" -> JsArray( addresses.map( encodeAddress ) ) ) :: Nil
              }
            }
            val f2 = fromBlock.fold( f1 ){ fb => ("fromBlock" -> fb.jsValue) :: f1 }
            val f3 = toBlock.fold( f2 ){ tb => ("toBlock" -> tb.jsValue) :: f2 }
            val f4 = {
              if ( topicRestrictionList.forall( _ == TopicRestriction.Any ) ) { // no restriction
                f3
              } else {
                ( "topics" -> JsArray( topicRestrictionList.map( _.jsValue ) ) ) :: f3
              }
            }
            f4.reverse
          }
          JsObject( fields )
        }
      }
    }
    final class Filter( val identifier : String ) extends Client.Filter

    def apply( rl : RawLog ) : Log = {
      val optionals = immutable.Seq.apply[Option[Any]]( rl.logIndex, rl.transactionIndex, rl.transactionHash, rl.blockHash, rl.blockNumber )
      val looksPending  = optionals.forall( _.isEmpty )
      val looksRecorded = optionals.forall( _.nonEmpty )

      val ele = EthLogEntry( rl.address, rl.topics, rl.data )

      val isRemoved = {
        rl.removed match {
          case Some( true ) => true
          case _            => false
        }
      }

      ( looksPending, looksRecorded, isRemoved ) match {
        case ( true, false, false ) => Pending( ele )
        case ( true, false, true )  => {
          throw new Exception( s"We should never see a removal of an event that was merely pending! Raw log: ${rl}" )
        }
        case ( false, true, false ) => {
          Recorded(
            logIndex = rl.logIndex.get,
            transactionIndex = rl.transactionIndex.get,
            transactionHash = rl.transactionHash.get,
            blockHash = rl.blockHash.get,
            blockNumber = rl.blockNumber.get,
            ethLogEntry = ele 
          )
        }
        case ( false, true, true ) => {
          Removed(
            logIndex = rl.logIndex.get,
            transactionIndex = rl.transactionIndex.get,
            transactionHash = rl.transactionHash.get,
            blockHash = rl.blockHash.get,
            blockNumber = rl.blockNumber.get,
            ethLogEntry = ele 
          )
        }
        case ( true, true, _ ) | ( false, false, _ ) => throw new InternalError( s"${optionals} can't both be nonEmpty and empty at the same time. Should never happen." )
      }
    }
    final case class Pending (
      val ethLogEntry : EthLogEntry
    ) extends Log

    final case class Removed (
      val logIndex         : Unsigned256,
      val transactionIndex : Unsigned256,
      val transactionHash  : EthHash,
      val blockHash        : EthHash,
      val blockNumber      : Unsigned256,
      val ethLogEntry      : EthLogEntry
    ) extends Log with Log.Full

    final case class Recorded (
      val logIndex         : Unsigned256,
      val transactionIndex : Unsigned256,
      val transactionHash  : EthHash,
      val blockHash        : EthHash,
      val blockNumber      : Unsigned256,
      val ethLogEntry      : EthLogEntry
    ) extends Log with Log.Full

    // we use a self-type rather than extending so matched on Log types don't
    // want Log.Full to be provably exhaustive
    trait Full {
      _ : Log => 
      def logIndex         : Unsigned256
      def transactionIndex : Unsigned256
      def transactionHash  : EthHash
      def blockHash        : EthHash
      def blockNumber      : Unsigned256
      def ethLogEntry      : EthLogEntry
    }
  }
  sealed trait Log {
    def ethLogEntry : EthLogEntry
  }

  case class TransactionReceipt (
    transactionHash   : EthHash,
    transactionIndex  : Unsigned256,
    blockHash         : EthHash,
    blockNumber       : Unsigned256,
    from              : Option[EthAddress],
    to                : Option[EthAddress],
    cumulativeGasUsed : Unsigned256,
    gasUsed           : Unsigned256,
    contractAddress   : Option[EthAddress],
    logs              : immutable.Seq[EthLogEntry],
    root              : Option[EthHash],
    status            : Option[Unsigned256]
  )

  val EmptyParams = Seq.empty[JsValue]

  trait eth {

    def blockNumber()( implicit ec : ExecutionContext ) : Future[BigInt]

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
    def getLogs( query : Log.Filter.Query )( implicit ec : ExecutionContext )                                    : Future[immutable.Seq[Client.Log]]
    def getLogs( filter : Log.Filter )( implicit ec : ExecutionContext )                                         : Future[immutable.Seq[Client.Log]]
    def getNewLogs( filter : Log.Filter )( implicit ec : ExecutionContext )                                      : Future[immutable.Seq[Client.Log]]
    def getTransactionCount( address : EthAddress, blockNumber : BlockNumber )( implicit ec : ExecutionContext ) : Future[BigInt]
    def getTransactionReceipt( transactionHash : EthHash )( implicit ec : ExecutionContext )                     : Future[Option[Client.TransactionReceipt]]
    def getBlockFilterChanges( filter : BlockFilter )( implicit ec : ExecutionContext )                          : Future[immutable.Seq[EthHash]]
    def newBlockFilter()( implicit ec : ExecutionContext )                                                       : Future[BlockFilter]
    def newLogFilter( query : Log.Filter.Query )( implicit ec : ExecutionContext )                               : Future[Log.Filter]
    def sendRawTransaction( bytes : Seq[Byte] )( implicit ec : ExecutionContext )                                : Future[EthHash]
    def sendSignedTransaction( signedTransaction : EthTransaction.Signed )( implicit ec : ExecutionContext )     : Future[EthHash]
    def uninstallFilter( filter : Filter )( implicit ec : ExecutionContext )                                     : Future[Boolean]
  }

  def forExchanger( exchanger : Exchanger ) : Client = new Client.Implementation.Exchanger( exchanger )

  def apply( url : URL )( implicit efactory : Exchanger.Factory = Exchanger.Factory.Default ) : Client = Client.forExchanger( efactory( Exchanger.Config( url ) ) )

  final object Implementation {
    final class Exchanger( exchanger : com.mchange.sc.v2.jsonrpc.Exchanger ) extends Client {
      def extractBigInt( success : Response.Success ) : BigInt = decodeQuantity( success.result.as[String] )

      def responseHandler[T]( onSuccess : Response.Success => T ) : Response => T = { result =>
        result match {
          case Yang( success ) => onSuccess( success )
          case Yin( error )    => error.vomit
        }
      }

      override def toString() : String = s"jsonrpc.Client.Implementation.Exchanger( ${exchanger} )"

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

        def blockNumber()( implicit ec : ExecutionContext ) : Future[BigInt] = {
          doExchange( "eth_blockNumber", EmptyParams )( success => decodeQuantity( success.result.as[String] ) )
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
        def getLogs( query : Log.Filter.Query )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Client.Log]] = {
          doExchange( "eth_getLogs", Seq( query.jsValue ) )( _.result.as[immutable.Seq[RawLog]].map( Client.Log.apply ) )
        }
        def getLogs( filter : Log.Filter )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Client.Log]] = {
          doExchange( "eth_getFilterLogs", Seq( JsString(filter.identifier) ) )( _.result.as[immutable.Seq[RawLog]].map( Client.Log.apply ) )
        }
        def getNewLogs( filter : Log.Filter )( implicit ec : ExecutionContext ) : Future[immutable.Seq[Client.Log]] = {
          doExchange( "eth_getFilterChanges", Seq( JsString(filter.identifier) ) )( _.result.as[immutable.Seq[RawLog]].map( Client.Log.apply ) )
        }
        def getTransactionReceipt( transactionHash : EthHash )( implicit ec : ExecutionContext ) : Future[Option[Client.TransactionReceipt]] = {
          val raw = doExchange( "eth_getTransactionReceipt", Seq( encodeBytes( transactionHash.bytes ) ) ) { success =>

            // handle Parity's weird habit of sometimes returning receipts for unmined transactions
            // JsUndefined covers both (usual) JsNull case and the possibility of an incomplete JsObject if blockHash is unknown
            def isMinedReceipt : Boolean = {
              (success.result \ "blockHash") match {
                case JsDefined(value) => value != JsNull
                case _ : JsUndefined  => false
              }
            }

            if (isMinedReceipt) success.result.as[Option[Client.TransactionReceipt]] else None
          }

          // geth 1.8.0 returns an undocumented error response, rather than a null success, on an unknown or pending transaction hash
          // we recover from this case. See https://github.com/ethereum/go-ethereum/issues/16092#issuecomment-366871447

          raw recover { case e : JsonrpcException =>
            if ( e.code == -32000 && e.message == "unknown transaction" ) None else throw e
          }
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
        def newLogFilter( query : Log.Filter.Query )( implicit ec : ExecutionContext ) : Future[Log.Filter] = {
          doExchange( "eth_newFilter", Seq( query.jsValue ) )( success => new Log.Filter( success.result.as[String] ) )
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
  }
}
trait Client extends AutoCloseable {
  def eth : Client.eth;

  def close()
}
