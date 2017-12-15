package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.EthLogEntry
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi

import scala.collection._

import java.io.StringWriter

import com.mchange.sc.v2.lang.borrow

import com.mchange.v2.io.IndentedWriter

import play.api.libs.json.Json


object Generator {

  private val StubImports = immutable.Seq(
    "scala.collection._",
    "scala.concurrent._",
    "scala.concurrent.duration.Duration",
    "com.mchange.sc.v2.concurrent.{Poller,Scheduler}",
    "com.mchange.sc.v2.net.URLSource",
    "com.mchange.sc.v2.failable._",
    "com.mchange.sc.v1.consuela._",
    "com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthLogEntry}",
    "com.mchange.sc.v1.consuela.ethereum.ethabi",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent",
    "com.mchange.sc.v1.consuela.ethereum.stub",
    "com.mchange.sc.v1.consuela.ethereum.stub.sol",
    "com.mchange.sc.v1.consuela.ethereum.stub.Utilities._",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Abi,Client}",
    "com.mchange.sc.v1.consuela.ethereum.rxblocks._",
    "play.api.libs.json.Json",
    "org.reactivestreams._"
  )

  private type OverloadedEventsMap = immutable.Map[String,immutable.Map[EthLogEntry.Topic,(String, Abi.Event)]]

  private val AnonymousEventName = "Anonymous"

  private val DefaultEventConfirmations = 12

  private def fillArgs( inputs : immutable.Seq[Abi.Function.Parameter] ) : immutable.Seq[Abi.Function.Parameter] = {
    inputs.zip( Stream.from(1) ).map { case ( param, index ) =>
      if ( param.name.length > 0 ) param else Abi.Function.Parameter( s"arg$index", param.`type` )
    }
  }
  private def fillInputs( fcn : Abi.Function ) = fcn.copy( inputs = fillArgs( fcn.inputs ) )

  // "continuePrint means expected to add to a print-ed, rather than println-ed
  // partial line, and print-s, rather than println-s its own final line

  def continuePrintMainConstructorArgs( withVal : Boolean, withEventConfirmations : Boolean, iw : IndentedWriter ) : Unit = {
    val mbv = if (withVal) "val " else ""
    val mbec = if ( withEventConfirmations) s", ${mbv}eventConfirmations : Int" else ""
    iw.println( s"( ${mbv}contractAddress : EthAddress${mbec} )( implicit" )
    iw.upIndent()
    iw.println( "icontext  : jsonrpc.Invoker.Context," )
    iw.println( "cfactory  : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    iw.println( "scheduler : Scheduler = Scheduler.Default," )
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.print( ")" )
  }

  def continuePrintAuxConstructorArgs( withEventConfirmations : Boolean, iw : IndentedWriter ) : Unit = {
    val mbec = if ( withEventConfirmations) ", eventConfirmations : Int" else ""
    iw.println( s"[T : URLSource, U : EthAddress.Source]( jsonRpcUrl : T, contractAddress : U${mbec} )( implicit" ) 
    iw.upIndent()
    iw.println( "cfactory  : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    iw.println( "scheduler : Scheduler = Scheduler.Default," )
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.print( ")" )
  }

  def generateFactoryMethods( className : String, iw : IndentedWriter ) : Unit = {
    iw.print( "def apply" )
    continuePrintMainConstructorArgs( false, true, iw )
    iw.println( s" : ${className} = {" )
    iw.upIndent()
    iw.println( s"new ${className}( contractAddress, eventConfirmations )( icontext, cfactory, poller, scheduler, econtext )" )
    iw.downIndent()
    iw.println( "}" )
    iw.println()
    iw.print( "def apply" )
    continuePrintMainConstructorArgs( false, false, iw )
    iw.println( s" : ${className} = {" )
    iw.upIndent()
    iw.println( s"this.apply( contractAddress, ${DefaultEventConfirmations} )" ) // we leave the implicits implicit, so we don't have to worry about compiler-generated evidence params
    iw.downIndent()
    iw.println( "}" )
    iw.println()
    iw.print( "def apply" )
    continuePrintAuxConstructorArgs( true, iw )
    iw.println( s" : ${className} = {" )
    iw.upIndent()
    iw.println( s"new ${className}(" )
    iw.upIndent()
    iw.println( "implicitly[EthAddress.Source[U]].toEthAddress( contractAddress )," )
    iw.println( "eventConfirmations" )
    iw.downIndent()
    iw.println( ") (" )
    iw.upIndent()
    iw.println( "jsonrpc.Invoker.Context( implicitly[URLSource[T]].toURL( jsonRpcUrl ).toExternalForm() )," )
    iw.println( "cfactory,"  )
    iw.println( "poller,"    )
    iw.println( "scheduler," )
    iw.println( "econtext"   )
    iw.downIndent()
    iw.println( ")" )
    iw.downIndent()
    iw.println( "}" )
    iw.print( "def apply" )
    continuePrintAuxConstructorArgs( false, iw )
    iw.println( s" : ${className} = {" )
    iw.upIndent()
    iw.println( s"this.apply( jsonRpcUrl, contractAddress, ${DefaultEventConfirmations} )" ) // we leave the implicits implicit, so we don't have to worry about compiler-generated evidence params
    iw.downIndent()
    iw.println( "}" )
  }

  /**
    * @return ( <generated class name>, <generated source file text> )
    */ 
  def generateContractStub( baseClassName : String, abi : Abi, async : Boolean, fullyQualifiedPackageName : String ) : (String, String) = {
    def asyncClassName = {
      val alwaysCapitalized = baseClassName(0).toString.toUpperCase + baseClassName.substring(1)
      s"Async${alwaysCapitalized}"
    }
    val className = if ( async ) asyncClassName else baseClassName

    val sw = new StringWriter()

    val overloadedEvents : OverloadedEventsMap = {
      def topicResolvedNameEvent( event : Abi.Event ) : ( EthLogEntry.Topic, (String, Abi.Event) ) = {
        ( SolidityEvent.computeIdentifierTopic( event ), (abiEventToResolvedName( event, abi ), event) )
      }
      val namesToOverloads = abi.events.filterNot( _.anonymous ).groupBy( _.name ).filter( _._2.length > 1 )
      namesToOverloads.mapValues( _.map( topicResolvedNameEvent ).toMap )
    }

    borrow( new IndentedWriter( sw, "  " ) ) { iw => // two-space indent, Scala-style
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()

      iw.println( s"final object $className {" )
      iw.upIndent()
      generateContractAbiVarFunctionsOverloadedEvents( overloadedEvents, abi, iw )
      generateTopLevelEventAndFactory( className, overloadedEvents, abi, iw )
      iw.println()
      generateFactoryMethods( className, iw )
      iw.downIndent()
      iw.println(  "}" )
      iw.print( s"final class $className" )
      continuePrintMainConstructorArgs( true, true, iw )
      iw.print(  s" extends Publisher[${className}.Event] {" )
      iw.upIndent()
      iw.println( s"final object transaction {" )
      iw.upIndent()
      abi.functions.foreach { fcn =>
        writeFunction( className, fillInputs(fcn), false, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println( s"final object constant {" )
      iw.upIndent()
      abi.functions.filter( _.constant ).foreach { fcn =>
        writeFunction( className, fillInputs(fcn), true, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println()
      writeEventsPublisher( className, abi, iw )
      iw.downIndent()
      iw.println( "}" )
    }

    (className, sw.toString)
  }

  private def forceHelper( solidityTypeName : String ) : ScalaParameterHelper = {
    scalaParameterHelperForSolidityType( solidityTypeName ).getOrElse {
      throw new StubException( s"No Scala type information available for solidity type '$solidityTypeName'." )
    }
  }

  private def scalaSignatureParam( param : Abi.Parameter ) : String = {
    val tpe = param.`type`
    val helper = forceHelper( tpe )
    s"${param.name} : ${helper.scalaTypeName}"
  }

  private def generateTopLevelEventAndFactory( className : String, overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    val hasAnonymous = abi.events.exists( _.anonymous )

    iw.println( "final object Event {" )
    iw.upIndent()
    writeAllEventDefinitions( className, abi, iw )
    iw.println()
    iw.println( "def apply( solidityEvent : SolidityEvent, metadata : stub.Event.Metadata ) : Event = {" )
    iw.upIndent()
    if ( hasAnonymous ) {
      generateNamedAnonymousEventSwitch( overloadedEvents, abi, iw )
    }
    else {
      iw.println( "val named = solidityEvent.asInstanceOf[SolidityEvent.Named]" )
      generateNamedEventSwitch( overloadedEvents, abi, iw )
    }
    iw.downIndent()
    iw.println( "}" )
    iw.println()
    generateEventProcessorClass(className, iw)
    iw.downIndent()
    iw.println( "}" )
    iw.println( "sealed trait Event extends stub.Event" )
    iw.println()
  }

  private def generateNamedAnonymousEventSwitch( overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( "solidityEvent match {" )
    iw.upIndent()
    iw.println( "case named : SolidityEvent.Named => {" )
    iw.upIndent()
    generateNamedEventSwitch(  overloadedEvents, abi, iw )
    iw.downIndent()
    iw.println( "}" )
    iw.println( s"case anonymous : SolidityEvent.Anonymous => Event.${AnonymousEventName}( anonymous, metadata )" )
    iw.downIndent()
    iw.println( "}" )
  }

  private def generateNamedEventSwitch( overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( "named.name match {" )
    iw.upIndent()

    val namedEvents = abi.events.filter( evt => !evt.anonymous )
    val notOverloaded = namedEvents.filterNot( e => overloadedEvents.keySet( e.name ) )
    notOverloaded.foreach { event =>
      iw.println( s"case \042${event.name}\042 => Event.${event.name}( named, metadata )" )
    }
    overloadedEvents.foreach { case ( name, innerMap ) =>
      innerMap.foreach { case ( topic, ( resolvedName, event ) ) =>
        iw.println( s"""case \042${name}\042 if (named.signatureTopic == ${overloadedEventSignatureTopicValName(event)}) => Event.${resolvedName}( named, metadata )""" )
      }
    }

    iw.downIndent()
    iw.println( "}" )
  }

  private def generateEventProcessorClass( className : String, iw : IndentedWriter ) : Unit = {
    iw.println( s"class Processor()(implicit scheduler : Scheduler, executionContext : ExecutionContext) extends SimpleProcessor[(SolidityEvent, stub.Event.Metadata ), ${className}.Event]()(scheduler, executionContext) {" )
    iw.upIndent()
    iw.println( s"def ftransform( pair : (SolidityEvent, stub.Event.Metadata) ) : Failable[${className}.Event] = succeed( ${className}.Event.apply( pair._1, pair._2 ) )" )
    iw.downIndent()
    iw.println( "}" )
  }

  private def generateContractAbiVarFunctionsOverloadedEvents( overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( s"val ContractAbi : Abi = Json.parse( \042\042\042${Json.stringify(Json.toJson(abi))}\042\042\042 ).as[Abi]" )
    abi.functions.zip( Stream.from(0) ).foreach { case ( fcn, index ) =>
      iw.println( s"val ${functionValName(fcn)} = ContractAbi.functions(${index})" )
    }
    for {
      name                             <- overloadedEvents.keySet.toSeq
      ( topic, (resolvedName, event) ) <- overloadedEvents.get( name ).fold( Nil : Seq[Tuple2[EthLogEntry.Topic,(String,Abi.Event)]] )( _.toSeq )
      
    } {
      iw.println( s"""val ${overloadedEventSignatureTopicValName(event)} = EthLogEntry.Topic("${topic.widen.hex}".decodeHex)""" )
    }
  }

  private def functionValName( f : Abi.Function ) : String = {
    val base = s"Function_${f.name}"
    val typesPart = f.inputs.map( _.`type` ).mkString("_")
    if ( typesPart.isEmpty ) base else s"${base}_${typesPart}"
  }

  private def overloadedEventSignatureTopicValName( e : Abi.Event ) : String = {
    val base = s"OverloadedEventSignatureTopic_${e.name}"
    val typesPart = e.inputs.map( _.`type` ).mkString("_")
    if ( typesPart.isEmpty ) base else s"${base}_${typesPart}"
  }

  /*
   * If there is just one anonymous event type defined, we know its type signature,
   * so we can define it as a typed event.
   * 
   * If there are multiple anonymous events defined, we can't easily know at runtime
   * which anonymous event we are encountering, so we defined a generic event wrapping
   * the raw log entry
   */ 
  private def writeAllEventDefinitions( stubClassName : String, abi : Abi, iw : IndentedWriter ) : Unit = {
    val ( named, anonymous ) = abi.events.partition( _.anonymous == false )
    named foreach { event =>
      val resolvedEventName = abiEventToResolvedName( event, abi )
      writeTypedEventDefinition( event, resolvedEventName, stubClassName, iw )
    }
    anonymous.length match {
      case 1 => writeTypedAnonymousEventDefinition( anonymous(0), stubClassName, iw )
      case _ => anonymous foreach { event =>
        writeUntypedAnonymousEventDefinition( stubClassName, iw )
      }
    }
  }


  private def writeEventsPublisher( className : String, abi : Abi, iw : IndentedWriter ) : Unit = {

    iw.println( "private lazy val eventProcessor = {" )
    iw.upIndent()
    iw.println(  "val eventsPublisher = new ConfirmedLogPublisher( icontext.jsonRpcUrl, Client.Log.Filter.Query( addresses = List(contractAddress) ), eventConfirmations )" )
    iw.println( s"val baseProcessor   = new StubEventProcessor( ${className}.ContractAbi )" )
    iw.println( s"val finalProcessor  = new ${className}.Event.Processor()" )
    iw.println(  "eventsPublisher.subscribe( baseProcessor )" )
    iw.println(  "baseProcessor.subscribe( finalProcessor )" )
    iw.println(  "finalProcessor" )
    iw.downIndent()
    iw.println( "}" )

    iw.println()
    iw.println( s"def subscribe( subscriber : Subscriber[_ >: ${className}.Event] ) = eventProcessor.subscribe( subscriber )")
    iw.println()
  }

  private def writeUntypedAnonymousEventDefinition (
    stubClassName : String,
    iw            : IndentedWriter
  ) : Unit = {
    iw.println( s"final object ${ AnonymousEventName } {" )
    iw.upIndent()
    iw.println( "def apply( solidityEvent : SolidityEvent.Anonymous, metadata : stub.Event.Metadata ) : ${ AnonymousEventName } = {" )
    iw.upIndent()
    iw.println( "this.apply (" )
    iw.upIndent()
    iw.println( "metadata," )
    iw.println( "solidityEvent.logEntry" )
    iw.downIndent()
    iw.println( ")" )
    iw.downIndent()
    iw.println( "}" )
    iw.downIndent()
    iw.println(  "}" )
    iw.println( s"final case class ${AnonymousEventName} (" )
    iw.upIndent()
    iw.println( "metadata : stub.Event.Metadata," )
    iw.println( "logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubClassName}.Event" )
    iw.println()
  }

  private def writeTypedAnonymousEventDefinition (
    event         : Abi.Event,
    stubClassName : String,
    iw            : IndentedWriter
  ) : Unit = {
    writeTypedEventDefinition( event, AnonymousEventName, stubClassName, iw ) 
  }

  private def writeTypedEventDefinition (
    event             : Abi.Event,
    resolvedEventName : String, // usually just event.name, but not for overloaded or typed anonymous events
    stubClassName     : String,
    iw                : IndentedWriter ) : Unit = {
    val helpers = event.inputs.map( input => forceHelper( input.`type` ) )
    val extractedParamValues = {
      for {
        i <- 0 until helpers.length
      } yield {
        helpers(i).outConversionGen( s"(solidityEvent.inputs( ${i} ).value)" )
      }
    }
    val solidityEventType = {
      if ( event.anonymous ) {
        "Anonymous"
      }
      else {
        "Named"
      }
    }
    iw.println( s"final object ${ resolvedEventName } {" )
    iw.upIndent()
    iw.println( s"def apply( solidityEvent : SolidityEvent.${solidityEventType}, metadata : stub.Event.Metadata ) : ${ resolvedEventName } = {" )
    iw.upIndent()
    iw.println( "this.apply (" )
    iw.upIndent()
    extractedParamValues.foreach { epv =>
      iw.println( epv + "," )
    }
    iw.println( "metadata," )
    iw.println( "solidityEvent.logEntry" )
    iw.downIndent()
    iw.println( ")" )
    iw.downIndent()
    iw.println( "}" )
    iw.downIndent()
    iw.println(  "}" )
    iw.println( s"final case class ${ resolvedEventName } (" )
    iw.upIndent()
    event.inputs.map( scalaSignatureParam ).map( _ + "," ).foreach( iw.println )
    iw.println( "metadata : stub.Event.Metadata," )
    iw.println( "logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubClassName}.Event" )
    iw.println()
  }

  private def writeFunction( className : String, fcn : Abi.Function, constantSection : Boolean, async : Boolean, iw : IndentedWriter ) : Unit = {
    def toRepLambda( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      helper.inConversionGen( param.name )
    }

    iw.println( functionSignature( fcn, constantSection, async ) + " = {" )
    iw.upIndent()

    if (! fcn.payable) {
      iw.println("val optionalPaymentInWei : Option[sol.UInt256] = None")
      iw.println()
    }
    iw.println( s"""val fcn = ${className}.${functionValName(fcn)}""" )
    iw.println( s"""val reps = immutable.Seq[Any]( ${fcn.inputs.map( toRepLambda ).mkString(", ")} )""" )
    iw.println( s"""val callData = ethabi.callDataForAbiFunctionFromEncoderRepresentations( reps, fcn ).get""" )

    if ( constantSection ) {
      iw.println( s"""val futRetBytes = jsonrpc.Invoker.constant.sendMessage( sender.address, contractAddress, optionalPaymentInWei.getOrElse( Zero ), callData )""" )
      iw.println( s"""val futDecodedReturnValues = futRetBytes.map( bytes => ethabi.decodeReturnValuesForFunction( bytes, fcn ) )""" )
      iw.println( s"""val futDecodedReps = futDecodedReturnValues.map( _.get.map( _.value  ).toVector )""" )

      iw.println( s"""val futOut = futDecodedReps.map { decodedReps =>""" )
      iw.upIndent()

      if ( fcn.outputs.length == 1 ) {
        val outHelper = forceHelper( fcn.outputs.head.`type` )
        iw.println( outHelper.outConversionGen( "decodedReps.head" ) )
      } else { // zero or multiple outputs, return Tuple (or Unit)
        val outHelpers = fcn.outputs.map( outparam => forceHelper( outparam.`type` ) )
        val indexedHelpers = immutable.Stream.from(1).zip(outHelpers)
        indexedHelpers.foreach { case ( index, helper ) =>
          iw.println( s"""val out${index} = ${helper.outConversionGen( "decodedReps( " + (index - 1) + " )" )}""" )
        }
        val indices = indexedHelpers.map( _._1 )
        iw.println( s"""( ${indices.map( index => "out" + index ).mkString(", ")} )""" )
      }

      iw.downIndent()
      iw.println( "}")
      if ( async ) {
        iw.println( "futOut" )
      } else {
        iw.println( s"""Await.result( futOut, Duration.Inf )""" )
      }
    } else {
      iw.println( s"""val futHash = jsonrpc.Invoker.transaction.sendMessage( sender.findSigner(), contractAddress, optionalPaymentInWei.getOrElse( Zero ), callData )""" )
      iw.println( s"""val futTransactionInfo = futHash.flatMap( hash => jsonrpc.Invoker.futureTransactionReceipt( hash ).map( mbctr => stub.TransactionInfo.Message.fromJsonrpcReceipt( hash, mbctr ) ) )""" )
      if ( async ) {
        iw.println( "futTransactionInfo" )
      } else {
        iw.println( s"""Await.result( futTransactionInfo, Duration.Inf )""" )
      }
    }


    iw.downIndent()
    iw.println( "}")
  }

  private def functionSignature( fcn : Abi.Function, constantSection : Boolean, async : Boolean ) : String = {
    def scalaType( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      s"${helper.scalaTypeName}"
    }

    def paymentArg = "optionalPaymentInWei : Option[sol.UInt256] = None"

    val params = fcn.inputs.map( scalaSignatureParam ) ++ ( if ( fcn.payable ) immutable.Seq( paymentArg ) else immutable.Seq.empty[String] )
    
    val prereturn = s"""def ${fcn.name}( ${params.mkString(", ")} )( implicit sender : stub.Sender )"""

    val post = {
      val raw = {
        if (!constantSection) {
          "stub.TransactionInfo.Message"
        } else {
          fcn.outputs.length match {
            case 0 => {
              "Unit"
            }
            case 1 => {
              scalaType( fcn.outputs.head )
            }
            case _ => {
              s"""( ${fcn.outputs.map( scalaType ).mkString(", ")} )"""
            }
          }
        }
      }
      " : " + (if ( async ) s"Future[ ${raw} ]" else raw)
    }

    prereturn + post
  }
}
