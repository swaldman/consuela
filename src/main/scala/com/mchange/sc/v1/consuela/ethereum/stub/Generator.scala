package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{ethabi,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi

import scala.collection._

import java.io.StringWriter

import com.mchange.sc.v2.lang.borrow

import com.mchange.v2.io.IndentedWriter

import com.mchange.sc.v1.log.MLevel._


import play.api.libs.json.Json


object Generator {

  final case class Generated( className : String, sourceCode : String )

  private implicit lazy val logger = mlogger( this )

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
    "com.mchange.sc.v1.consuela.ethereum.ethabi.{Decoded,SolidityEvent}",
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

  private def fillArgs( inputs : immutable.Seq[Abi.Function.Parameter] ) : immutable.Seq[Abi.Function.Parameter] = {
    inputs.zip( Stream.from(1) ).map { case ( param, index ) =>
      if ( param.name.length > 0 ) param else Abi.Function.Parameter( s"arg$index", param.`type` )
    }
  }
  private def fillInputs( fcn : Abi.Function ) = fcn.copy( inputs = fillArgs( fcn.inputs ) )

  private def eventsNoEvents[T]( abi : Abi )( ifEvents : =>T, ifNoEvents : =>T ) = {
    if (abi.events.nonEmpty) ifEvents else ifNoEvents
  }
  private def ifEvents( abi : Abi )( action : =>Unit ) : Unit = eventsNoEvents(abi)( action, () )

  def generateFactoryMethods( className : String, abi : Abi, iw : IndentedWriter ) : Unit = {
    val mbEventConfirmations = eventsNoEvents(abi)( ", eventConfirmations : Int = stub.DefaultEventConfirmations", "" )
    iw.println( s"def apply[U : EthAddress.Source]( contractAddress : U${mbEventConfirmations} ) ( implicit" )
    iw.upIndent()
    iw.println( "icontext  : jsonrpc.Invoker.Context,"  )
    iw.println( "cfactory  : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    ifEvents(abi) {
      iw.println( "scheduler : Scheduler = Scheduler.Default," )
    }
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s") : ${className} = {" )
    iw.upIndent()
    val mbEventConfirmationCtorArg = eventsNoEvents(abi)( ", eventConfirmations", "" )
    iw.println( s"new ${className}( implicitly[EthAddress.Source[U]].toEthAddress( contractAddress )${mbEventConfirmationCtorArg} )" )
    iw.downIndent()
    iw.println( "}" )
    iw.println()
    iw.print( "def build[T : URLSource, U : EthAddress.Source] (" )
    iw.upIndent()
    iw.println( "jsonRpcUrl : T,")
    iw.println( "contractAddress : U," )
    ifEvents(abi) {
      iw.println( "eventConfirmations : Int = stub.DefaultEventConfirmations," )
    }
    iw.println( "gasPriceTweak : stub.MarkupOrOverride = stub.MarkupOrOverride.None," )
    iw.println( "gasLimitTweak : stub.MarkupOrOverride = stub.DefaultGasLimitMarkup," )
    iw.println( "pollPeriod : Duration = stub.DefaultPollPeriod," )
    iw.println( "pollTimeout : Duration = stub.DefaultPollTimeout" )
    iw.downIndent()
    iw.println( ")( implicit" )
    iw.upIndent()
    iw.println( "cfactory  : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    ifEvents(abi) {
      iw.println( "scheduler : Scheduler = Scheduler.Default," )
    }
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s") : ${className} = {" )
    iw.upIndent()
    iw.println( s"new ${className}(" )
    iw.upIndent()
    iw.println( "implicitly[EthAddress.Source[U]].toEthAddress( contractAddress )" )
    ifEvents(abi) {
      iw.println( ", eventConfirmations" )
    }
    iw.downIndent()
    iw.println( ") (" )
    iw.upIndent()
    iw.println( "jsonrpc.Invoker.Context( implicitly[URLSource[T]].toURL( jsonRpcUrl ).toExternalForm(), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout )," )
    iw.println( "cfactory,"  )
    iw.println( "poller,"    )
    ifEvents(abi) {
      iw.println( "scheduler," )
    }
    iw.println( "econtext"   )
    iw.downIndent()
    iw.println( ")" )
    iw.downIndent()
    iw.println( "}" )
  }

  private def stubUtilitiesClassName( baseClassName : String ) = {
      val alwaysCapitalized = baseClassName(0).toString.toUpperCase + baseClassName.substring(1)
      s"${alwaysCapitalized}Utilities"
  }

  def generateStubClasses( baseClassName : String, abi : Abi, fullyQualifiedPackageName : String ) : immutable.Set[Generated] = {
    immutable.Set(
      generateStubUtilities( baseClassName, abi, fullyQualifiedPackageName ),
      generateContractStub( baseClassName, abi, true, fullyQualifiedPackageName ),
      generateContractStub( baseClassName, abi, false, fullyQualifiedPackageName )
    )
  }

  private def generateStubUtilities( baseClassName : String, abi : Abi, fullyQualifiedPackageName : String ) : Generated = {

    val className = stubUtilitiesClassName( baseClassName )

    // this will naturally be empty if there are no events
    val overloadedEvents : OverloadedEventsMap = {
      def topicResolvedNameEvent( event : Abi.Event ) : ( EthLogEntry.Topic, (String, Abi.Event) ) = {
        ( SolidityEvent.computeIdentifierTopic( event ), (abiEventToResolvedName( event, abi ), event) )
      }
      val namesToOverloads = abi.events.filterNot( _.anonymous ).groupBy( _.name ).filter( _._2.length > 1 )
      namesToOverloads.mapValues( _.map( topicResolvedNameEvent ).toMap )
    }

    val sw = new StringWriter()

    borrow( new IndentedWriter( sw, "  " ) ) { iw => // two-space indent, Scala-style
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()

      iw.println( s"object ${className} {" )
      iw.upIndent()

      generateContractAbiAndFunctionsVals( abi, iw )
      ifEvents( abi ) {
        generateEventTopicVals( overloadedEvents, abi, iw )
        generateTopLevelEventAndFactory( className, overloadedEvents, abi, iw )
      }

      iw.downIndent()
      iw.println(  "}" )
    }

    Generated( className, sw.toString )
  }

  private def generateContractStub( baseClassName : String, abi : Abi, async : Boolean, fullyQualifiedPackageName : String ) : Generated = {
    def asyncClassName = {
      val alwaysCapitalized = baseClassName(0).toString.toUpperCase + baseClassName.substring(1)
      s"Async${alwaysCapitalized}"
    }
    val className = if ( async ) asyncClassName else baseClassName

    val stubUtilitiesClass = stubUtilitiesClassName( baseClassName )

    val sw = new StringWriter()

    borrow( new IndentedWriter( sw, "  " ) ) { iw => // two-space indent, Scala-style
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()

      iw.println( s"final object ${className} {" )
      iw.upIndent()
      ifEvents( abi ) {
        iw.println( s"val  Event = ${stubUtilitiesClass}.Event" )
        iw.println( s"type Event = ${stubUtilitiesClass}.Event" )
        iw.println()
      }
      generateFactoryMethods( className, abi, iw )
      iw.downIndent()
      iw.println(  "}" )
      val mbEventConfirmations = eventsNoEvents( abi )( ", val eventConfirmations : Int = stub.DefaultEventConfirmations", "" )
      val mbExtends = eventsNoEvents( abi )( s"extends Publisher[${className}.Event]", "" )
      iw.println( s"final class $className( val contractAddress : EthAddress${mbEventConfirmations} )( implicit" )
      iw.upIndent()
      iw.println( "icontext  : jsonrpc.Invoker.Context," )
      iw.println( "cfactory  : jsonrpc.Client.Factory = jsonrpc.Client.Factory.Default,"  )
      iw.println( "poller    : Poller = Poller.Default," )
      ifEvents( abi ) {
        iw.println( "scheduler : Scheduler = Scheduler.Default," )
      }
      iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
      iw.downIndent()
      iw.println( s")${mbExtends} {" )
      iw.upIndent()
      iw.println()
      iw.println( "val gasPriceTweak : stub.MarkupOrOverride = icontext.gasPriceTweak" )
      iw.println( "val gasLimitTweak : stub.MarkupOrOverride = icontext.gasLimitTweak" )
      iw.println( "val pollPeriod : Duration = icontext.pollPeriod" )
      iw.println( "val pollTimeout : Duration = icontext.pollTimeout" )
      iw.println()
      iw.println( s"final object transaction {" )
      iw.upIndent()
      abi.functions.foreach { fcn =>
        writeFunction( className, stubUtilitiesClass, fillInputs(fcn), false, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println( s"final object constant {" )
      iw.upIndent()
      abi.functions.filter( _.constant ).foreach { fcn =>
        writeFunction( className, stubUtilitiesClass, fillInputs(fcn), true, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println()
      ifEvents(abi) {
        writeEventsPublisher( className, stubUtilitiesClass, abi, iw )
      }
      iw.downIndent()
      iw.println( "}" )
    }

    Generated(className, sw.toString)
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

  private def isDynamicLength( solidityType : String ) : Boolean = ethabi.solidityTypeIsDynamicLength( solidityType ).xwarn( s"No encoder found for solidity type '${ solidityType }'" ).get

  private def isHashEncoded( param : Abi.Event.Parameter ) : Boolean = param.indexed && isDynamicLength( param.`type` )

  private def eventScalaSignatureParam( param : Abi.Event.Parameter ) : String = {
    if ( isHashEncoded( param ) ) s"${param.name} : EthHash" else scalaSignatureParam( param )
  }


  private def generateNamedEventSpecializedPublisher( resolvedName : String, overloadedEvents : OverloadedEventsMap, event : Abi.Event, iw : IndentedWriter ) : Unit = {
    val indexedInputs = event.inputs.filter( _.indexed )
    val indexedCount  = indexedInputs.length

    assert( !event.anonymous, "Anonymous event passed to the generator of a named event." )

    if ( indexedCount > 3 ) {
      throw new StubException( s"Bad event in ABI, named events may not have more than three indexed parameters (after the hash of the event signature)! ${event}" )
    }

    def indexedParam( param : Abi.Event.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      s"${param.name} : Seq[${helper.scalaTypeName}] = Nil"
    }
    def indexedParamElementEncoder( param : Abi.Event.Parameter ) : String => String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      def unwrappedEncodedValueProducer : String => String = value => s"""ethabi.Encoder.encoderForSolidityType( "${tpe}" ).get.encodeUntyped( ${ helper.inConversionGen( value ) } ).get"""
      def wrappedEncodedValueProducer : String => String = value => s"""EthLogEntry.Topic( ${unwrappedEncodedValueProducer(value)} )"""
      def hashedEncodedValueProducer : String => String = value => s"""EthLogEntry.Topic( EthHash.hash( ${unwrappedEncodedValueProducer(value)} ).bytes )"""
      if ( isDynamicLength( tpe ) ) {
        hashedEncodedValueProducer
      }
      else {
        wrappedEncodedValueProducer
      }
    }
    def indexedParamEncoder( param : Abi.Event.Parameter ) : String => String = {
      val each = indexedParamElementEncoder( param )( "value" )
      seqParam => s"${seqParam}.map( value => ${each} )"
    }
    val params = "address : Seq[T] = (Nil : List[String])" +: indexedInputs.map( indexedParam ) :+ "eventConfirmations : Int = stub.DefaultEventConfirmations"


    iw.println( "final object Publisher {" )
    iw.upIndent()

    iw.println( s"class Processor()(implicit scheduler : Scheduler, executionContext : ExecutionContext) extends SimpleProcessor[(SolidityEvent, stub.Event.Metadata ), Event.${resolvedName}]()(scheduler, executionContext) {" )
    iw.upIndent()

    iw.println( s"override def ftransform( pair : (SolidityEvent, stub.Event.Metadata) ) : Failable[Event.${resolvedName}] = succeed( Event.${resolvedName}.apply( pair._1.asInstanceOf[SolidityEvent.Named], pair._2 ) )" )

    iw.downIndent()
    iw.println( "}" )

    iw.println()

    iw.println( "def build[T : EthAddress.Source, U : URLSource](" )
    iw.upIndent()
    iw.println( "jsonRpcUrl : U," )
    params.foreach { param =>
      iw.println( s"${param}," )
    }
    iw.println( "gasPriceTweak : stub.MarkupOrOverride = stub.MarkupOrOverride.None," )
    iw.println( "gasLimitTweak : stub.MarkupOrOverride = stub.DefaultGasLimitMarkup," )
    iw.println( "pollPeriod : Duration = stub.DefaultPollPeriod," )
    iw.println( "pollTimeout : Duration = stub.DefaultPollTimeout" )
    iw.downIndent()
    iw.println( ")( implicit" )
    iw.upIndent()
    iw.println( "cfactory         : Client.Factory   = Client.Factory.Default," )
    iw.println( "scheduler        : Scheduler        = Scheduler.Default," )
    iw.println( "executionContext : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s""") : Publisher[Event.${resolvedName}] = {""" )
    iw.upIndent()
    iw.println( "implicit val icontext = jsonrpc.Invoker.Context( implicitly[URLSource[U]].toURL( jsonRpcUrl ).toExternalForm(), gasPriceTweak, gasLimitTweak, pollPeriod, pollTimeout )" )
    val applyArgs = "address" +: indexedInputs.map( _.name ) :+ "eventConfirmations"
    iw.println( s"""this.apply( ${applyArgs.mkString(", ")} )""" )
    iw.downIndent()
    iw.println( "}" )

    iw.println()

    iw.println( s"""def apply[T : EthAddress.Source]( ${params.mkString(", ")} )(implicit""" )
    iw.upIndent()
    iw.println( "icontext         : jsonrpc.Invoker.Context," )
    iw.println( "cfactory         : Client.Factory   = Client.Factory.Default," )
    iw.println( "scheduler        : Scheduler        = Scheduler.Default," )
    iw.println( "executionContext : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s""") : Publisher[Event.${resolvedName}] = {""" )
    iw.upIndent()

    iw.println(  "val addresses = address.map( implicitly[EthAddress.Source[T]].toEthAddress )" )
    iw.println( s"val signatureRestriction = Client.Log.Filter.TopicRestriction.Exact( ${anyEventSignatureTopicValName( overloadedEvents, event )} )" )

    val queryInputs : immutable.Seq[String]= {
      val indexedInputsNumbered = indexedInputs.zip( Stream.from(2) )
      val userRestrictions : immutable.Seq[String] = indexedInputsNumbered.map { case ( param, num ) =>
        val topicSeq = indexedParamEncoder( param )( param.name )
        s"restriction_${num} = topicRestriction( ${topicSeq} )"
      }
      Vector( "addresses = addresses", "restriction_1 = signatureRestriction" ) ++ userRestrictions
    }

    iw.println( s"val query = Client.Log.Filter.Query(" )
    iw.upIndent()

    queryInputs.init.foreach( qi => iw.println( qi + "," ) )
    iw.println( queryInputs.last )

    iw.downIndent()
    iw.println( s")" )

    iw.println(  "val eventsPublisher = new ConfirmedLogPublisher( icontext.jsonRpcUrl, query, eventConfirmations )" )
    iw.println(  "val baseProcessor   = new StubEventProcessor( ContractAbi )" )
    iw.println( s"val finalProcessor  = new Event.${resolvedName}.Publisher.Processor()" )
    iw.println(  "eventsPublisher.subscribe( baseProcessor )" )
    iw.println(  "baseProcessor.subscribe( finalProcessor )" )
    iw.println(  "finalProcessor" )

    iw.downIndent()
    iw.println( "}" )

    iw.downIndent()
    iw.println( "}" )
  }
    
  private def generateTopLevelEventAndFactory( stubUtilitiesClassName : String, overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    val hasAnonymous = abi.events.exists( _.anonymous )

    iw.println( "final object Event {" )
    iw.upIndent()
    writeAllEventDefinitions( stubUtilitiesClassName, overloadedEvents, abi, iw )
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
    generateEventProcessorClass( stubUtilitiesClassName, iw )
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

  private def generateEventProcessorClass( stubUtilitiesClassName : String, iw : IndentedWriter ) : Unit = {
    iw.println(
      s"class Processor()(implicit scheduler : Scheduler, executionContext : ExecutionContext) extends SimpleProcessor[(SolidityEvent, stub.Event.Metadata ), ${stubUtilitiesClassName}.this.Event]()(scheduler, executionContext) {"
    )
    iw.upIndent()
    iw.println( s"override def ftransform( pair : (SolidityEvent, stub.Event.Metadata) ) : Failable[${stubUtilitiesClassName}.this.Event] = succeed( ${stubUtilitiesClassName}.this.Event.apply( pair._1, pair._2 ) )" )
    iw.downIndent()
    iw.println( "}" )
  }
  
  private def generateContractAbiAndFunctionsVals( abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( s"val ContractAbi : Abi = Json.parse( \042\042\042${Json.stringify(Json.toJson(abi))}\042\042\042 ).as[Abi]" )
    abi.functions.zip( Stream.from(0) ).foreach { case ( fcn, index ) =>
      iw.println( s"val ${functionValName(fcn)} = ContractAbi.functions(${index})" )
    }
  }


  private def generateEventTopicVals( overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    for {
      name                             <- overloadedEvents.keySet.toSeq
      ( topic, (resolvedName, event) ) <- overloadedEvents.get( name ).fold( Nil : Seq[Tuple2[EthLogEntry.Topic,(String,Abi.Event)]] )( _.toSeq )
      
    } {
      iw.println( s"""val ${overloadedEventSignatureTopicValName(event)} = EthLogEntry.Topic("${topic.widen.hex}".decodeHex)""" )
    }
    abi.events.filterNot( event => overloadedEvents.keySet( event.name ) ).foreach { event =>
      iw.println( s"""val ${nonOverloadedEventSignatureTopicValName( event )} = EthLogEntry.Topic("${SolidityEvent.computeIdentifierTopic( event ).widen.hex}".decodeHex)""" )
    }
  }

  private val SolidityArrayRegex = """^(.+?)\[(\d*)\](.*)$""".r

  private def sanitizeSolidityType( solidityTypeName : String ) : String = {
    solidityTypeName match {
      case SolidityArrayRegex( prefix, len, suffix ) => sanitizeSolidityType( prefix + "$array" + len + "$" + suffix )
      case _ => solidityTypeName
    }
  }

  private def sanitizedType( param : Abi.Parameter ) : String = sanitizeSolidityType( param.`type` )

  private def functionValName( f : Abi.Function ) : String = {
    val base = s"Function_${f.name}"
    val typesPart = f.inputs.map( sanitizedType ).mkString("_")
    if ( typesPart.isEmpty ) base else s"${base}_${typesPart}"
  }

  private def overloadedEventSignatureTopicValName( e : Abi.Event ) : String = {
    val base = s"OverloadedEventSignatureTopic_${e.name}"
    val typesPart = e.inputs.map( sanitizedType ).mkString("_")
    if ( typesPart.isEmpty ) base else s"${base}_${typesPart}"
  }


  private def nonOverloadedEventSignatureTopicValName( e : Abi.Event ) : String = {
    val base = s"EventSignatureTopic_${e.name}"
    val typesPart = e.inputs.map( sanitizedType ).mkString("_")
    if ( typesPart.isEmpty ) base else s"${base}_${typesPart}"
  }

  private def anyEventSignatureTopicValName( overloadedEvents : OverloadedEventsMap, e : Abi.Event ) : String = {
    if ( overloadedEvents.keySet( e.name ) ) {
      overloadedEventSignatureTopicValName( e )
    }
    else {
      nonOverloadedEventSignatureTopicValName( e )
    }
  }
  
  /*
   * If there is just one anonymous event type defined, we know its type signature,
   * so we can define it as a typed event.
   * 
   * If there are multiple anonymous events defined, we can't easily know at runtime
   * which anonymous event we are encountering, so we defined a generic event wrapping
   * the raw log entry
   */ 
  private def writeAllEventDefinitions( stubUtilitiesClassName : String, overloadedEvents : OverloadedEventsMap, abi : Abi, iw : IndentedWriter ) : Unit = {
    val ( named, anonymous ) = abi.events.partition( _.anonymous == false )
    named foreach { event =>
      val resolvedEventName = abiEventToResolvedName( event, abi )
      writeTypedEventDefinition( stubUtilitiesClassName, event, overloadedEvents, resolvedEventName, iw )
    }
    anonymous.length match {
      case 1 => writeTypedAnonymousEventDefinition( stubUtilitiesClassName, anonymous(0), overloadedEvents, iw )
      case _ => anonymous foreach { event =>
        writeUntypedAnonymousEventDefinition( stubUtilitiesClassName, iw )
      }
    }
  }


  private def writeEventsPublisher( className : String, stubUtilitiesClassName : String, abi : Abi, iw : IndentedWriter ) : Unit = {

    iw.println( "private lazy val eventProcessor = {" )
    iw.upIndent()
    iw.println(  "val eventsPublisher = new ConfirmedLogPublisher( icontext.jsonRpcUrl, Client.Log.Filter.Query( addresses = List(contractAddress) ), eventConfirmations )" )
    iw.println( s"val baseProcessor   = new StubEventProcessor( ${stubUtilitiesClassName}.ContractAbi )" )
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

  private def writeUntypedAnonymousEventDefinition ( stubUtilitiesClassName : String, iw : IndentedWriter ) : Unit = {
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
    iw.println( s") extends ${stubUtilitiesClassName}.this.Event" )
    iw.println()
  }

  private def writeTypedAnonymousEventDefinition (
    stubUtilitiesClassName : String,
    event                      : Abi.Event,
    overloadedEvents           : OverloadedEventsMap,
    iw                         : IndentedWriter
  ) : Unit = {
    writeTypedEventDefinition( stubUtilitiesClassName, event, overloadedEvents, AnonymousEventName, iw ) 
  }

  private def writeTypedEventDefinition (
    stubUtilitiesClassName : String,
    event                      : Abi.Event,
    overloadedEvents           : OverloadedEventsMap,
    resolvedEventName          : String, // usually just event.name, but not for overloaded or typed anonymous events
    iw                         : IndentedWriter
  ) : Unit = {
    val helpers = event.inputs.map( input => forceHelper( input.`type` ) )
    val extractedParamValues = {
      for {
        (input, i) <- event.inputs.zip( Stream.from(0) )
      } yield {
        if ( isHashEncoded( input ) ) {
          s"(solidityEvent.inputs( ${i} ).asInstanceOf[Decoded.Hash].hash)"
        }
        else {
          forceHelper( input.`type` ).outConversionGen( s"(solidityEvent.inputs( ${i} ).asInstanceOf[Decoded.Value].value)" )
        }
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

    if (!event.anonymous) {
      iw.println( "val abiEvent = solidityEvent.abiEvent" )
      iw.println( "val solidityEventSignatureTopic = SolidityEvent.computeIdentifierTopic( abiEvent )" )
      iw.println( s"""require( solidityEventSignatureTopic == ${anyEventSignatureTopicValName( overloadedEvents, event )}, s"${resolvedEventName} represents the following ABI event: '${event}' SolidityEvent '$${solidityEvent}' represents an ABI event that does not match." )""" )
    }
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
    if ( !event.anonymous ) {
      generateNamedEventSpecializedPublisher( resolvedEventName, overloadedEvents, event, iw  )
    }
    iw.downIndent()
    iw.println(  "}" )
    iw.println( s"final case class ${ resolvedEventName } (" )
    iw.upIndent()
    event.inputs.map( eventScalaSignatureParam ).map( _ + "," ).foreach( iw.println )
    iw.println( "metadata : stub.Event.Metadata," )
    iw.println( "logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubUtilitiesClassName}.this.Event" )
    iw.println()
  }

  private def writeFunction( className : String, stubUtilitiesClassName : String, fcn : Abi.Function, constantSection : Boolean, async : Boolean, iw : IndentedWriter ) : Unit = {
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
    iw.println( s"""val fcn = ${stubUtilitiesClassName}.${functionValName(fcn)}""" )
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
