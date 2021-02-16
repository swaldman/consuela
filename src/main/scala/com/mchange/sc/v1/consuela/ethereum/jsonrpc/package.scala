package com.mchange.sc.v1.consuela.ethereum

import play.api.libs.json._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.specification.Types.{ByteSeqExact32,Unsigned256}
import com.mchange.sc.v2.playjson._

import scala.collection._
import scala.util.control.NonFatal

package object jsonrpc {

  final class BadAbiException( message : String, cause : Throwable = null ) extends ConsuelaException( message, cause )

  private[jsonrpc] def encodeQuantity( quantity : BigInt )  : JsString = JsString( "0x" + quantity.toString(16) )

  private[jsonrpc] def decodeQuantity( encoded : JsString ) : BigInt = decodeQuantity( encoded.value )

  private[jsonrpc] def decodeQuantity( encoded : String ) : BigInt = {
    require( encoded.startsWith("0x"), s"Ethereum JSON-RPC expects hex-encoded quantities to begin with '0x', '${encoded}' does not." )
    BigInt( encoded.substring(2), 16 )
  }

  private[jsonrpc] def encodeBytes( bytes : Seq[Byte] )  : JsString = JsString( "0x" + bytes.hex )

  private[jsonrpc] def decodeBytes( encoded : JsString ) : immutable.Seq[Byte] = decodeBytes( encoded.value )

  private[jsonrpc] def decodeBytes( encoded : String ) : immutable.Seq[Byte] = {
    try {
      encoded.decodeHex.toImmutableSeq
    }
    catch { 
      case e : NumberFormatException => { // handle an annoying special case that shows up in ganache...
        if ( encoded == "0x0" ) Nil else throw e
      }
    }
  }

  private[jsonrpc] def encodeAddress( address : EthAddress ) : JsString = encodeBytes( address.bytes.widen )

  trait MaybeEmpty {
    def isEmpty : Boolean
  }

  implicit val EthLogEntryFormat = new Format[EthLogEntry] {
    def reads( jsv : JsValue ) : JsResult[EthLogEntry] = jsv match {
      case jso : JsObject => {
        try {
          val fields = jso.value
          val address = EthAddress( fields( "address" ).as[String] )
          val topics = fields( "topics" ).as[JsArray].value.map( jsv => ByteSeqExact32( decodeBytes( jsv.as[String] ) ) ).toVector
          val data = decodeBytes( fields( "data" ).as[String] )
          JsSuccess( EthLogEntry( address, topics, data ) )
        } catch {
          case NonFatal( t ) => JsError( t.toString() )
        }
      }
      case _ => JsError( s"EthLogEntry expected as a JsObject, found ${jsv}" )
    }
    def writes( entry : EthLogEntry ) : JsValue = {
      JsObject( immutable.Map( "address" -> encodeBytes( entry.address.bytes.widen ), "topics" -> JsArray( entry.topics.map( topic => encodeBytes( topic.widen ) ) ), "data" -> encodeBytes( entry.data ) ) )
    }
  }

  implicit val EthHashFormat = new Format[EthHash] {
    def reads( jsv : JsValue ) : JsResult[EthHash] = {
      try {
        JsSuccess( EthHash.withBytes( decodeBytes( jsv.as[String] ) ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( hash : EthHash ) : JsValue = encodeBytes( hash.bytes )
  }
  implicit val EthAddressFormat = new Format[EthAddress] {
    def reads( jsv : JsValue ) : JsResult[EthAddress] = {
      try {
        JsSuccess( EthAddress( jsv.as[String] ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( address : EthAddress ) : JsValue = encodeBytes( address.bytes.widen )
  }
  implicit val Unsigned256Format = new Format[Unsigned256] {
    def reads( jsv : JsValue ) : JsResult[Unsigned256] = {
      try {
        JsSuccess( Unsigned256( decodeQuantity( jsv.as[String] ) ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( num : Unsigned256 ) : JsValue = encodeQuantity( num.widen )
  }
  implicit val ClientTransactionReceiptFormat = Json.format[Client.TransactionReceipt]

  implicit val ByteDataFormat = new Format[immutable.Seq[Byte]] {
    def reads( jsv : JsValue ) : JsResult[immutable.Seq[Byte]] = {
      try {
        JsSuccess( decodeBytes( jsv.as[String] ) )
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( data : immutable.Seq[Byte] ) : JsValue = encodeBytes( data )
  }

  // See EthLogEntry.Topic
  implicit val TopicsFormat = new Format[immutable.IndexedSeq[ByteSeqExact32]] {
    def reads( jsv : JsValue ) : JsResult[immutable.IndexedSeq[ByteSeqExact32]] = {
      try {
        JsSuccess( jsv.as[JsArray].value.toVector.map( elem => ByteSeqExact32( decodeBytes( elem.as[String] ) ) ) )
      }
      catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( data : immutable.IndexedSeq[ByteSeqExact32] ) : JsValue = JsArray( data.map( bse => encodeBytes(bse.widen) ) )
  }


  class OptionFormat[T : Format] extends Format[Option[T]] {
    def reads( jsv : JsValue ) : JsResult[Option[T]] = {
      try {
        JsSuccess {
          if ( jsv == JsNull ) None else Some( jsv.as[T] )
        }
      } catch {
        case NonFatal( t ) => JsError( t.toString() )
      }
    }
    def writes( mbReceipt : Option[T] ) : JsValue = mbReceipt match {
      case Some( receipt ) => Json.toJson( receipt )
      case None            => JsNull
    }
  }

  implicit def toOptionFormat[T : Format] = new OptionFormat[T]

  implicit val UserMethodInfoFormat          = Json.format[Compilation.Doc.User.MethodInfo]
  implicit val DeveloperMethodInfoFormat     = Json.format[Compilation.Doc.Developer.MethodInfo]
  implicit val UserDocFormat                 = Json.format[Compilation.Doc.User]
  implicit val DeveloperDocFormat            = Json.format[Compilation.Doc.Developer]

  implicit val CompilationContractInfoFormat = Json.format[Compilation.Contract.Info]

  // I have no idea why trying to generate this Format automatically was very fragile,
  // sometimes working, sometimes failing
  //
  // The manual version seems to work reliably

  //implicit val CompilationContractFormat = Json.format[Compilation.Contract]

  implicit val CompilationContractFormat : Format[Compilation.Contract] = new Format[Compilation.Contract] {
    def reads( jsv : JsValue ) : JsResult[Compilation.Contract] = {
      try {
        jsv match {
          case jso : JsObject => {
            val code = jso.value.get( "code" ).getOrElse( throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should contain a 'code' key, does not: ${jso}" ) ).as[String]
            val info = jso.value.get( "info" ).getOrElse( throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should contain a 'info' key, does not: ${jso}" ) ).as[Compilation.Contract.Info]
            JsSuccess( Compilation.Contract( code, info ) )
          }
          case other => throw new ConsuelaException( s"The JSON encoding of a jsonrpc.Compilation.Contract should be a JsObject, is not: ${other}" )
        }
      }
      catch {
        case e : Exception => JsError( e.getMessage() )
      }
    }
    def writes( contract : Compilation.Contract ) : JsValue = JsObject( "code" -> JsString(contract.code) :: "info" -> Json.toJson(contract.info) :: Nil )
  }

  implicit val MapStringCompilationContractFormat = new Format[immutable.Map[String,Compilation.Contract]] {
    def reads( jsv : JsValue ) : JsResult[immutable.Map[String,Compilation.Contract]] = {
      jsv match {
        case jso : JsObject => {
          jso.fields.foldLeft( JsSuccess(immutable.Map.empty[String,Compilation.Contract]) : JsResult[immutable.Map[String,Compilation.Contract]] ){
            (jsr, pair) => jsr.flatMap( map => Json.fromJson[Compilation.Contract](pair._2).flatMap( info => JsSuccess( map + Tuple2( pair._1, info ) ) ) )
          }
        }
        case _ => JsError( s"Map of contract name to Compilation.Contract expected as a JsObject, found ${jsv}" )
      }
    }
    def writes( definition : immutable.Map[String,Compilation.Contract] ) : JsValue = {
      JsObject( definition.map{ case ( k , v ) => ( k , Json.toJson(v) ) } )
    }
  }

  // we have to leave this in the package object,
  // since type declarations can't be top level
  type Compilation = immutable.Map[String,Compilation.Contract]
}
