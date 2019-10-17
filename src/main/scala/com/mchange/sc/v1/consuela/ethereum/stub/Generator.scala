package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{ethabi,EthLogEntry}
import com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi

import scala.collection._

import java.io.{File,StringWriter}

import com.mchange.sc.v2.lang.borrow

import com.mchange.sc.v3.failable.logging._

import com.mchange.v2.io.IndentedWriter

import com.mchange.sc.v1.log.MLevel._

import play.api.libs.json.Json


object Generator {

  final case class Generated( className : String, sourceCode : String )

  final object Regenerated {
    final case class Updated( targetFile : File, sourceCode : String ) extends Regenerated
    final case class Unchanged( targetFile : File ) extends Regenerated

    def updated( targetFile : File, g : Generated ) : Updated = Updated( targetFile, g.sourceCode )
    def unchanged( targetFile : File ) : Unchanged = Unchanged( targetFile )
  }
  trait Regenerated {
    def targetFile : File
  }

  private implicit lazy val logger = mlogger( this )

  private val StubImports = immutable.Seq(
    "java.net.URL",
    "scala.collection._",
    "scala.concurrent._",
    "scala.concurrent.duration.Duration",
    "scala.util.Try",
    "com.mchange.sc.v3.failable._",
    "com.mchange.sc.v2.concurrent.{Poller,Scheduler}",
    "com.mchange.sc.v2.jsonrpc.Exchanger",
    "com.mchange.sc.v2.net.{LoadBalancer,URLSource}",
    "com.mchange.sc.v1.consuela._",
    "com.mchange.sc.v1.log.MLevel._",
    "com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthChainId,EthHash,EthLogEntry}",
    "com.mchange.sc.v1.consuela.ethereum.ethabi",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.Encoder.ArrayRep",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.{Decoded,SolidityEvent}",
    "com.mchange.sc.v1.consuela.ethereum.stub",
    "com.mchange.sc.v1.consuela.ethereum.stub.{sol,Nonce,Payment,UnexpectedEventException}",
    "com.mchange.sc.v1.consuela.ethereum.stub.Utilities._",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc.{Abi,Client}",
    "com.mchange.sc.v1.consuela.ethereum.rxblocks._",
    "play.api.libs.json.Json",
    "org.reactivestreams._"
  )

  private type OverloadedEventsMap = immutable.Map[String,immutable.Map[EthLogEntry.Topic,(String, Abi.Event)]]

  private val AnonymousEventName = "Anonymous"

  private def expandArgs( inputs : immutable.Seq[Abi.Function.Parameter] ) : immutable.Seq[Abi.Function.Parameter] = {
    inputs.zip( Stream.from(1) ).map { case ( param, index ) =>
      if ( param.name.length > 0 ) param else Abi.Function.Parameter( s"arg$index", param.`type` )
    }
  }
  private def fillMissingInputArgs( fcn : Abi.Function ) = fcn.copy( inputs = expandArgs( fcn.inputs ) )

  private val ExtraArgListArgs = immutable.Set("sender")

  private def transformInputArgs( fcn : Abi.Function, sas : immutable.SortedSet[SyntheticArg] ) : Abi.Function = {
    val filledFunction = fillMissingInputArgs( fcn )

    val origNameSet = fcn.inputs.map( _.name ).toSet
    val synthNameSet = sas.map( _.inSignature ) ++ ExtraArgListArgs

    def uniquify( param : Abi.Function.Parameter ) : Abi.Function.Parameter = {
      val avoidSet = (origNameSet - param.name) ++ synthNameSet
      var goodName = param.name
      while ( avoidSet( goodName ) ) goodName = s"_${goodName}"
      param.copy( name = goodName )
    }

    filledFunction.copy( inputs = filledFunction.inputs.map( uniquify ) )
  }

  private def eventsNoEvents[T]( abi : Abi )( ifEvents : =>T, ifNoEvents : =>T ) = {
    if (abi.events.nonEmpty) ifEvents else ifNoEvents
  }
  private def ifEvents( abi : Abi )( action : =>Unit ) : Unit = eventsNoEvents(abi)( action, () )

  def generateFactoryMethods( className : String, abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( s"def apply[U : EthAddress.Source]( contractAddress : U ) ( implicit" )
    iw.upIndent()
    iw.println( "scontext  : stub.Context"  )
    iw.downIndent()
    iw.println( s") : ${className} = {" )
    iw.upIndent()
    iw.println( s"new ${className}( implicitly[EthAddress.Source[U]].toEthAddress( contractAddress ) )( scontext )" )
    iw.downIndent()
    iw.println( "}" )

    iw.println()
    iw.print( "def build[T : EthAddress.Source, U : URLSource] (" )
    iw.upIndent()
    iw.println( "jsonRpcUrl : U,")
    iw.println( "contractAddress : T," )
    iw.println( "chainId : Option[EthChainId] = stub.Context.Default.ChainId," )
    iw.println( "gasPriceTweak : stub.MarkupOrOverride = stub.Context.Default.GasPriceTweak," )
    iw.println( "gasLimitTweak : stub.MarkupOrOverride = stub.Context.Default.GasLimitTweak," )
    iw.println( "pollPeriod : Duration = stub.Context.Default.PollPeriod," )
    iw.println( "pollTimeout : Duration = stub.Context.Default.PollTimeout," )
    iw.println( "httpTimeout : Duration = stub.Context.Default.HttpTimeout," )
    iw.println( "transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover," )
    iw.println( "transactionLogger : stub.TransactionLogger = stub.Context.Default.TransactionLogger," )
    iw.println( "eventConfirmations : Int = stub.Context.Default.EventConfirmations," )
    iw.println( "onTransactionSubmitted : Try[EthHash] => Unit = stub.Context.Default.OnTransactionSubmitted" )
    iw.downIndent()
    iw.println( ")( implicit" )
    iw.upIndent()
    iw.println( "efactory  : Exchanger.Factory = Exchanger.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    iw.println( "scheduler : Scheduler = Scheduler.Default," )
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s") : ${className} = {" )
    iw.upIndent()
    iw.println( "this.buildLoadBalanced(" )
    iw.upIndent()
    iw.println( "loadBalancer = LoadBalancer.Single( jsonRpcUrl )," )
    iw.println( "contractAddress = contractAddress," )
    iw.println( "chainId = chainId," )
    iw.println( "gasPriceTweak = gasPriceTweak," )
    iw.println( "gasLimitTweak = gasLimitTweak," )
    iw.println( "pollPeriod = pollPeriod," )
    iw.println( "pollTimeout = pollTimeout," )
    iw.println( "httpTimeout = httpTimeout," )
    iw.println( "transactionApprover = transactionApprover," )
    iw.println( "transactionLogger = transactionLogger," )
    iw.println( "eventConfirmations = eventConfirmations," )
    iw.println( "onTransactionSubmitted = onTransactionSubmitted" )
    iw.downIndent()
    iw.println( ")( implicitly[EthAddress.Source[T]], efactory, poller, scheduler, econtext )" )
    iw.downIndent()
    iw.println(  "}" )

    iw.println()
    iw.print( "def buildLoadBalanced[T : EthAddress.Source] (" )
    iw.upIndent()
    iw.println( "loadBalancer : LoadBalancer,")
    iw.println( "contractAddress : T," )
    iw.println( "chainId : Option[EthChainId] = stub.Context.Default.ChainId," )
    iw.println( "gasPriceTweak : stub.MarkupOrOverride = stub.Context.Default.GasPriceTweak," )
    iw.println( "gasLimitTweak : stub.MarkupOrOverride = stub.Context.Default.GasLimitTweak," )
    iw.println( "pollPeriod : Duration = stub.Context.Default.PollPeriod," )
    iw.println( "pollTimeout : Duration = stub.Context.Default.PollTimeout," )
    iw.println( "httpTimeout : Duration = stub.Context.Default.HttpTimeout," )
    iw.println( "transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover," )
    iw.println( "transactionLogger : stub.TransactionLogger = stub.Context.Default.TransactionLogger," )
    iw.println( "eventConfirmations : Int = stub.Context.Default.EventConfirmations," )
    iw.println( "onTransactionSubmitted : Try[EthHash] => Unit = stub.Context.Default.OnTransactionSubmitted" )
    iw.downIndent()
    iw.println( ")( implicit" )
    iw.upIndent()
    iw.println( "efactory  : Exchanger.Factory = Exchanger.Factory.Default,"  )
    iw.println( "poller    : Poller = Poller.Default," )
    iw.println( "scheduler : Scheduler = Scheduler.Default," )
    iw.println( "econtext  : ExecutionContext = ExecutionContext.global" )
    iw.downIndent()
    iw.println( s") : ${className} = {" )
    iw.upIndent()
    iw.println( "val scontext = {")
    iw.upIndent()
    iw.println( "stub.Context.fromLoadBalancer(" )
    iw.upIndent()
    iw.println( "loadBalancer = loadBalancer," )
    iw.println( "chainId = chainId," )
    iw.println( "gasPriceTweak = gasPriceTweak," )
    iw.println( "gasLimitTweak = gasLimitTweak," )
    iw.println( "pollPeriod = pollPeriod," )
    iw.println( "pollTimeout = pollTimeout," )
    iw.println( "httpTimeout = httpTimeout," )
    iw.println( "transactionApprover = transactionApprover," )
    iw.println( "transactionLogger = transactionLogger," )
    iw.println( "eventConfirmations = eventConfirmations," )
    iw.println( "onTransactionSubmitted = onTransactionSubmitted" )
    iw.downIndent()
    iw.println( ")( efactory, poller, scheduler )")
    iw.downIndent()
    iw.println( "}")
    iw.print( s"new ${className}( implicitly[EthAddress.Source[T]].toEthAddress( contractAddress ) )( scontext )" )
    iw.downIndent()
    iw.println( "}" )
  }

  private def stubUtilitiesClassName( baseClassName : String ) = {
      val alwaysCapitalized = baseClassName(0).toString.toUpperCase + baseClassName.substring(1)
      s"${alwaysCapitalized}Utilities"
  }

  private def allGeneratedClassNames( baseClassName : String ) : immutable.Set[String] = {
    immutable.Set( stubUtilitiesClassName( baseClassName ), asyncClassName( baseClassName ), baseClassName )
  }

  private def shouldRegenerate( targetFile : File, abiTimestamp : Option[Long] ) : Boolean = {
    if ( targetFile.exists() ) {
      abiTimestamp match {
        case Some( abiTime ) => {
          abiTime > targetFile.lastModified
        }
        case None => true
      }
    }
    else {
      true
    }
  }

  def regenerateStubClasses( destDir : File, baseClassName : String, fullyQualifiedPackageName : String, abi : Abi, abiTimestamp : Option[Long] ) : immutable.Set[Regenerated] = {
    def f( className : String ) : File = new File( destDir, className + ".scala" )
    def regenerated( targetFile : File, generator : => Generated ) : Regenerated = if ( shouldRegenerate( targetFile, abiTimestamp ) ) Regenerated.updated( targetFile, generator ) else Regenerated.unchanged( targetFile )

    val stubUtilitiesFile = f( stubUtilitiesClassName( baseClassName )  )
    val asyncStubFile = f( asyncClassName( baseClassName ) )
    val syncStubFile = f( baseClassName )

    immutable.Set(
      regenerated( stubUtilitiesFile, generateStubUtilities( baseClassName, abi, fullyQualifiedPackageName ) ),
      regenerated( asyncStubFile, generateContractStub( baseClassName, abi, true, fullyQualifiedPackageName ) ),
      regenerated( syncStubFile, generateContractStub( baseClassName, abi, false, fullyQualifiedPackageName ) )
    )
  }

  def generateStubClasses( baseClassName : String, abi : Abi, fullyQualifiedPackageName : String ) : immutable.Set[Generated] = {
    immutable.Set(
      generateStubUtilities( baseClassName, abi, fullyQualifiedPackageName ),
      generateContractStub( baseClassName, abi, true, fullyQualifiedPackageName ),
      generateContractStub( baseClassName, abi, false, fullyQualifiedPackageName )
    )
  }

  private def generateAutogeneratedComment( iw : IndentedWriter ) : Unit = {
    val comment = {
      s"""|/*
          | * This class autogenerated by ${ this.getClass.getName }
          | *
          | * ${new java.util.Date}
          | *
          | * DO NOT HAND EDIT! ANY CHANGES WILL LIKELY BE OVERWRITTEN.
          | */""".stripMargin
    }
    iw.println( comment )
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
      generateAutogeneratedComment( iw )
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()

      iw.println( s"object ${className} {" )
      iw.upIndent()

      iw.println( "private implicit lazy val logger = mlogger( this )" )
      iw.println()

      generateContractAbiAndEventFunctionsVals( abi, iw )
      ifEvents( abi ) {
        generateEventTopicVals( overloadedEvents, abi, iw )
        generateTopLevelEventAndFactory( className, overloadedEvents, abi, iw )
      }

      iw.downIndent()
      iw.println(  "}" )
    }

    Generated( className, sw.toString )
  }

  private def asyncClassName( baseClassName : String ) : String = {
    val alwaysCapitalized = baseClassName(0).toString.toUpperCase + baseClassName.substring(1)
    s"Async${alwaysCapitalized}"
  }

  private def generateContractStub( baseClassName : String, abi : Abi, async : Boolean, fullyQualifiedPackageName : String ) : Generated = {

    val className = if ( async ) asyncClassName( baseClassName ) else baseClassName

    val stubUtilitiesClass = stubUtilitiesClassName( baseClassName )

    val sw = new StringWriter()

    borrow( new IndentedWriter( sw, "  " ) ) { iw => // two-space indent, Scala-style
      generateAutogeneratedComment( iw )
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
      val mbExtends = eventsNoEvents( abi )( s" extends Publisher[${className}.Event]", "" )
      iw.println( s"final class $className( val contractAddress : EthAddress )( implicit scontext : stub.Context )${mbExtends} {" )
      iw.upIndent()
      iw.println()
      iw.println( "val chainId                : Option[EthChainId]       = scontext.icontext.chainId" )
      iw.println( "val gasPriceTweak          : stub.MarkupOrOverride    = scontext.icontext.gasPriceTweak" )
      iw.println( "val gasLimitTweak          : stub.MarkupOrOverride    = scontext.icontext.gasLimitTweak" )
      iw.println( "val pollPeriod             : Duration                 = scontext.icontext.pollPeriod" )
      iw.println( "val pollTimeout            : Duration                 = scontext.icontext.pollTimeout" )
      iw.println( "val httpTimeout            : Duration                 = scontext.icontext.httpTimeout" )
      iw.println( "val transactionApprover    : stub.TransactionApprover = scontext.icontext.transactionApprover" )
      iw.println( "val transactionLogger      : stub.TransactionLogger   = scontext.icontext.transactionLogger" )
      iw.println( "val eventConfirmations     : Int                      = scontext.eventConfirmations" )
      iw.println( "val onTransactionSubmitted : Try[EthHash] => Unit     = scontext.onTransactionSubmitted" )
      iw.println()
      iw.println( "def address = contractAddress" )
      iw.println()
      iw.println( "// Conservatively ensure the desired implicit environment" ) // Conservatively ensure the desired implicit environment
      iw.println( "implicit val efactory  : Exchanger.Factory        = scontext.icontext.efactory" )
      iw.println( "implicit val poller    : Poller                   = scontext.icontext.poller" )
      iw.println( "implicit val econtext  : ExecutionContext         = scontext.icontext.econtext" )
      iw.println( "implicit val scheduler : Scheduler                = scontext.scheduler" )
      iw.println( "implicit val icontext  : jsonrpc.Invoker.Context  = scontext.icontext" )
      iw.println()
      iw.println( s"final object transaction {" )
      iw.upIndent()
      abi.functions.foreach { fcn =>
        val sas = syntheticArgSet( fcn, false )
        val xf_fcn = transformInputArgs(fcn, sas)
        writeFunction( className, stubUtilitiesClass, xf_fcn, false, async, sas, iw )
        writeSyntheticArgumentFunctionOverloads( sas, xf_fcn, false, async, iw )
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println( s"final object constant {" )
      iw.upIndent()
      abi.functions.filter( _.constant ).foreach { fcn =>
        val sas = syntheticArgSet( fcn, true )
        val xf_fcn = transformInputArgs(fcn, sas)
        writeFunction( className, stubUtilitiesClass, xf_fcn, true, async, sas, iw )
        writeSyntheticArgumentFunctionOverloads( sas, xf_fcn, true, async, iw )
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println()
      ifEvents(abi) {
        writeEventsPublisher( className, stubUtilitiesClass, abi, iw )
      }
      iw.println()
      iw.println("// subobject aliases")
      iw.println("val view = constant")
      iw.println("val txn  = transaction")
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
    val nonStubContextParams = "address : Seq[T] = (Nil : List[String])" +: indexedInputs.map( indexedParam )
    val allParams = "jsonRpcUrl : U" +: nonStubContextParams :+ "eventConfirmations : Int = stub.Context.Default.EventConfirmations"

    iw.println( "final object Publisher {" )
    iw.upIndent()

    iw.println( s"class Processor()(implicit scheduler : Scheduler, executionContext : ExecutionContext) extends SimpleProcessor[(SolidityEvent, stub.Event.Metadata ), Event.${resolvedName}]()(scheduler, executionContext) {" )
    iw.upIndent()

    iw.println( s"override def ftransform( pair : (SolidityEvent, stub.Event.Metadata) ) : Failable[Event.${resolvedName}] = Failable.succeed( Event.${resolvedName}.apply( assertNamedEvent(pair._1, pair._2), pair._2 ) )" )

    iw.downIndent()
    iw.println( "}" )

    iw.println()

    iw.println( s"""def fromStubContext[T : EthAddress.Source]( ${nonStubContextParams.mkString(", ")} )( implicit scontext : stub.Context ) : Publisher[Event.${resolvedName}] = {""" )
    iw.upIndent()
    val fromUrlArgs = "scontext.icontext.loadBalancer.nextURL" +: "address" +: indexedInputs.map( _.name ) :+ "scontext.eventConfirmations"
    iw.println( s"""this.fromUrl( ${fromUrlArgs.mkString(", ")} )( implicitly[EthAddress.Source[T]], implicitly[URLSource[URL]], scontext.scheduler, scontext.icontext.econtext )""" )
    iw.downIndent()

    iw.println(  """}""" )

    iw.println( s"""def fromUrl[T : EthAddress.Source, U : URLSource]( ${allParams.mkString(", ")} )(implicit scheduler : Scheduler, executionContext : ExecutionContext) : Publisher[Event.${resolvedName}] = {""" )
    iw.upIndent()

    iw.println(  "val addresses = address.map( implicitly[EthAddress.Source[T]].toEthAddress )" )
    iw.println(  "val url = URLSource.toURL( jsonRpcUrl ).toExternalForm()" )
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

    iw.println(  "val eventsPublisher = new ConfirmedLogPublisher( url, query, eventConfirmations )" )
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
    iw.println()
    iw.println( s"def collect( info : stub.TransactionInfo ) : immutable.Seq[${stubUtilitiesClassName}.Event] = stub.Event.collectForAbi( ContractAbi, info, this.apply _ )" )
    iw.println()
    iw.println( s"def collect( info : stub.TransactionInfo.Async )( implicit ec : ExecutionContext ) : Future[immutable.Seq[${stubUtilitiesClassName}.Event]] = stub.Event.collectForAbi( ContractAbi, info, this.apply _ )" )
    iw.println()
    writeAllEventDefinitions( stubUtilitiesClassName, overloadedEvents, abi, iw )
    iw.println()
    iw.println( "def apply( solidityEvent : SolidityEvent, metadata : stub.Event.Metadata ) : Event = {" )
    iw.upIndent()
    if ( hasAnonymous ) {
      generateNamedAnonymousEventSwitch( overloadedEvents, abi, iw )
    }
    else {
      iw.println( "val named = assertNamedEvent( solidityEvent, metadata )" )
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
    iw.println( """case unexpected => throw new UnexpectedEventException( named, metadata, s"Event with unexpected name (or, if overloaded, unexpected signature): ${named}" )""" )

    iw.downIndent()
    iw.println( "}" )
  }

  private def generateEventProcessorClass( stubUtilitiesClassName : String, iw : IndentedWriter ) : Unit = {
    iw.println(
      s"class Processor()(implicit scheduler : Scheduler, executionContext : ExecutionContext) extends SimpleProcessor[(SolidityEvent, stub.Event.Metadata ), ${stubUtilitiesClassName}.this.Event]()(scheduler, executionContext) {"
    )
    iw.upIndent()
    iw.println( s"override def ftransform( pair : (SolidityEvent, stub.Event.Metadata) ) : Failable[${stubUtilitiesClassName}.this.Event] = Failable.succeed( ${stubUtilitiesClassName}.this.Event.apply( pair._1, pair._2 ) )" )
    iw.downIndent()
    iw.println( "}" )
  }
  
  private def generateContractAbiAndEventFunctionsVals( abi : Abi, iw : IndentedWriter ) : Unit = {
    iw.println( s"val ContractAbi : Abi = Json.parse( \042\042\042${Json.stringify(Json.toJson(abi))}\042\042\042 ).as[Abi]" )
    abi.functions.zip( Stream.from(0) ).foreach { case ( fcn, index ) =>
      iw.println( s"val ${functionValName(fcn)} = ContractAbi.functions(${index})" )
    }
    abi.events.zip( Stream.from(0) ).foreach { case ( event, index ) =>
      iw.println( s"val ${eventValName(event,abi)} = ContractAbi.events(${index})" )
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

  private def eventValName( event : Abi.Event, abi : Abi ) : String = {
    val resolvedEventName = abiEventToResolvedName( event, abi )
    eventValName( event, resolvedEventName )
  }

  private def eventValName( event : Abi.Event, resolvedEventName : String ) : String = {
    val base = s"Event_${resolvedEventName}"
    val typesPart = event.inputs.map( sanitizedType ).mkString("_")
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
    iw.println(  "val eventsPublisher = new ConfirmedLogPublisher( icontext.loadBalancer.nextURL.toExternalForm(), Client.Log.Filter.Query( addresses = List(contractAddress) ), eventConfirmations )" )
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
    stubUtilitiesClassName     : String,
    event                      : Abi.Event,
    overloadedEvents           : OverloadedEventsMap,
    iw                         : IndentedWriter
  ) : Unit = {
    writeTypedEventDefinition( stubUtilitiesClassName, event, overloadedEvents, AnonymousEventName, iw ) 
  }

  private def writeTypedEventDefinition (
    stubUtilitiesClassName     : String,
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
    extractedParamValues.length match {
      case 0 => /* ignore */
      case 1 => iw.println( extractedParamValues.head )
      case _ => {
        extractedParamValues.init.foreach { epv =>
          iw.println( epv + "," )
        }
        iw.println( extractedParamValues.last )
      }
    }
    iw.downIndent()
    iw.println(")(")
    iw.upIndent()
    iw.println( "metadata," )
    iw.println( "solidityEvent.logEntry" )
    iw.downIndent()
    iw.println( ")" )
    iw.downIndent()
    iw.println( "}" )
    if ( !event.anonymous ) {
      iw.println( s"private def fromLog( recordedLog : Client.Log.Recorded ) : Failable[${ resolvedEventName }] = {" )
      iw.upIndent()
      iw.println( s"val event = ${eventValName( event, resolvedEventName )}" )
      iw.println(  "val logEntry = recordedLog.ethLogEntry" )
      iw.println(  "val metadata = stub.Event.Metadata( recordedLog )" )
      iw.println( s"SolidityEvent.interpretLogEntryAsEvent( logEntry, event ).map( inputs => SolidityEvent.Named( inputs, logEntry, event ) ).map( se => ${ resolvedEventName }.apply( se, metadata ) )" )
      iw.downIndent()
      iw.println(  "}" )
      iw.println()
      iw.println( s"def fetch( addresses : Seq[EthAddress], fromBlock : Option[stub.BlockNumber], toBlock : Option[stub.BlockNumber] )( implicit scontext : stub.Context ) : Future[immutable.Seq[${resolvedEventName}]] = {" )
      iw.upIndent()
      iw.println(  "implicit val ec = scontext.icontext.econtext" )
      iw.println( s"val signatureRestriction = Client.Log.Filter.TopicRestriction.Exact( ${anyEventSignatureTopicValName( overloadedEvents, event )} )" )
      iw.println(  "val query = Client.Log.Filter.Query( addresses = addresses, fromBlock = fromBlock, toBlock = toBlock, restriction_1 = signatureRestriction )" )
      iw.println(  "val flogs = jsonrpc.Invoker.getLogs( query )( scontext.icontext )" )
      iw.println(  "flogs.map( seq => seq.partition( _.isInstanceOf[Client.Log.Recorded] ) ).map { case ( recorded, others ) =>" )
      iw.upIndent()
      iw.println( """others.foreach( o => WARNING.log( s"Ignored unrecorded log: ${o}" ) )""" )
      iw.println( s"val f_fetched = recorded.map( rec => ${resolvedEventName}.fromLog( rec.asInstanceOf[Client.Log.Recorded] ) )" )
      iw.println(  "val ( succeeded, failed ) = f_fetched.zip( recorded ).partition( _._1.isSucceeded )" )
      iw.println(  "failed.foreach { case ( failure, log ) => " )
      iw.upIndent()
      iw.println( """val logMe = s"Despite a topic indicating a matching event name and types, log '${log}' could not be interpreted as a """ + resolvedEventName + """, and will be skipped."""" )
      iw.println( """val failed = failure.assertFailed""" )
      iw.println( """WARNING.log( logMe + s" Failure message: '${failed.message}', Failure source: '${failed.source.toString}'" )""" )
      iw.downIndent()
      iw.println("}")
      iw.println( "Failable.sequence( succeeded.map( _._1 ) ).get" )
      iw.downIndent()
      iw.println( "}" )
      iw.downIndent()
      iw.println( "}" )
      iw.println()
      iw.println( s"def collect( info : stub.TransactionInfo ) : immutable.Seq[${resolvedEventName}] = {" )
      iw.upIndent()
      iw.println( s"${stubUtilitiesClassName}.Event.collect( info ).filter( _.isInstanceOf[${resolvedEventName}] ).map( _.asInstanceOf[${resolvedEventName}] )" )
      iw.downIndent()
      iw.println(  "}" )
      iw.println()
      iw.println( s"def collect( info : stub.TransactionInfo.Async )( implicit ec : ExecutionContext ) : Future[immutable.Seq[${resolvedEventName}]] = info.future.map( collect )" )
      iw.println()
      generateNamedEventSpecializedPublisher( resolvedEventName, overloadedEvents, event, iw  )
    }
    iw.downIndent()
    iw.println(  "}" )
    iw.println( s"final case class ${ resolvedEventName } (" )
    iw.upIndent()
    val signatureParams = event.inputs.map( eventScalaSignatureParam )
    signatureParams.length match {
      case 0 => /* ignore */
      case 1 => iw.println( signatureParams.head )
      case _ => {
        signatureParams.init.map( _ + "," ).foreach( iw.println )
        iw.println( signatureParams.last )
      }
    }
    iw.downIndent()
    iw.println( ")(" )
    iw.upIndent()
    iw.println( "val metadata : stub.Event.Metadata," )
    iw.println( "val logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubUtilitiesClassName}.this.Event" )
    iw.println()
  }

  private def writeFunction( className : String, stubUtilitiesClassName : String, fcn : Abi.Function, constantSection : Boolean, async : Boolean, syntheticArgs : immutable.SortedSet[SyntheticArg], iw : IndentedWriter ) : Unit = {
    def toRepLambda( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      helper.inConversionGen( param.name )
    }

    iw.println( functionSignature( fcn, constantSection, async, syntheticArgs ) + " = {" )
    iw.upIndent()

    if (! fcn.payable) {
      iw.println("val payment : Payment = Payment.None")
      iw.println()
    }
    iw.println( s"""val fcn = ${stubUtilitiesClassName}.${functionValName(fcn)}""" )
    iw.println( s"""val reps = immutable.Seq[Any]( ${fcn.inputs.map( toRepLambda ).mkString(", ")} )""" )
    iw.println(  """val callData = ethabi.callDataForAbiFunctionFromEncoderRepresentations( reps, fcn ).get""" )

    if ( constantSection ) {
      iw.println( """val futRetBytes = jsonrpc.Invoker.constant.sendMessage( sender.address, contractAddress, payment.amountInWei, callData )""" )
      iw.println( """val futDecodedReturnValues = futRetBytes.map( bytes => ethabi.decodeReturnValuesForFunction( bytes, fcn ) )""" )
      iw.println( """val futDecodedReps = futDecodedReturnValues.map( _.get.map( _.value  ).toVector )""" )

      iw.println( """val futOut = futDecodedReps.map { decodedReps =>""" )
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
      iw.println( """val futHash = jsonrpc.Invoker.transaction.sendMessage( sender.findSigner(), contractAddress, payment.amountInWei, callData, nonce.toOption )""" )
      iw.println( """futHash.onComplete( onTransactionSubmitted )""" )
      iw.println( """val futTransactionInfoAsync = futHash.map( hash => stub.TransactionInfo.Async.fromClientTransactionReceipt( hash, jsonrpc.Invoker.futureTransactionReceipt( hash ) ) )""" )
      if ( async ) {
        iw.println( "futTransactionInfoAsync" )
      } else {
        iw.println( s"""Await.result( futTransactionInfoAsync, Duration.Inf ).await""" )
      }
    }


    iw.downIndent()
    iw.println( "}")
  }

  private object SyntheticArg {
    val Payment = SyntheticArg( 0, "payment : Payment", "payment", "Payment.None" )
    val Nonce   = SyntheticArg( 1, "nonce : Nonce", "nonce", "Nonce.Auto" )

    implicit val IndexOrdering : Ordering[SyntheticArg] = Ordering.by( _.index )
    
  }
  private case class SyntheticArg( index : Int, inSignature : String, inCall : String, defaultValue : String )

  private def syntheticArgSet( fcn : Abi.Function, constantSection : Boolean ) : immutable.SortedSet[SyntheticArg] = {
    ( fcn.payable, constantSection ) match {
      case ( true, true )   => immutable.SortedSet( SyntheticArg.Payment )
      case ( false, true )  => immutable.SortedSet.empty[SyntheticArg]
      case ( true, false )  => immutable.SortedSet( SyntheticArg.Payment, SyntheticArg.Nonce )
      case ( false, false ) => immutable.SortedSet( SyntheticArg.Nonce )
    }
  }

  private def overloadArgSets( mainArgSet : immutable.SortedSet[SyntheticArg] ) : immutable.Set[immutable.SortedSet[SyntheticArg]] = mainArgSet.subsets.toSet - mainArgSet

  /*
   * Initially we used default arguments to make provision of the synthetic arguments optional
   * but that won't be robust to smart contracts with overloaded solidity functions.
   * 
   * (In Scala mixing default arguments and function overloads is fragile.)
   * 
   */ 
  private def writeSyntheticArgumentFunctionOverloads( mainArgSet : immutable.SortedSet[SyntheticArg], fcn : Abi.Function, constantSection : Boolean, async : Boolean, iw : IndentedWriter ) : Unit = {
    overloadArgSets( mainArgSet ).foreach { overloadArgSet =>
      writeSyntheticArgumentFunctionOverload( fcn, constantSection, async, mainArgSet, overloadArgSet, iw )
    }
  }

  private def writeSyntheticArgumentFunctionOverload(
    fcn : Abi.Function,
    constantSection : Boolean,
    async : Boolean,
    mainArgSet : immutable.SortedSet[SyntheticArg],
    overloadArgSet : immutable.SortedSet[SyntheticArg],
    iw : IndentedWriter
  ) : Unit = {
    val sig = functionSignature( fcn, constantSection, async, overloadArgSet )

    def findCallValue( arg : SyntheticArg ) = if ( overloadArgSet( arg ) ) arg.inCall else arg.defaultValue

    val synthCallArgs = mainArgSet.toSeq.map( findCallValue ) // if we don't "toSeq" it first, it re-sorts by a String ordering... grrrr.
    val callArgs = fcn.inputs.map( _.name ) ++ synthCallArgs
    val callArgsStr = callArgs.mkString( ", " )
    val call = s"${fcn.name}( ${callArgsStr} )( sender )"

    iw.println( s"${sig} = {" )
    iw.upIndent()
    iw.println( call )
    iw.downIndent()
    iw.println(  "}")
  }

  private def functionSignature( fcn : Abi.Function, constantSection : Boolean, async : Boolean, syntheticArgs : immutable.SortedSet[SyntheticArg] ) : String = {
    def scalaType( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      s"${helper.scalaTypeName}"
    }

    // the toSeq is important, otherwise the synthetic args re-sort by some String ordering
    val params = fcn.inputs.map( scalaSignatureParam ) ++ syntheticArgs.toSeq.map( _.inSignature )

    val senderPart = if ( constantSection ) "( implicit sender : stub.Sender )" else "( implicit sender : stub.Sender.Signing )"
    
    val prereturn = s"""def ${fcn.name}( ${params.mkString(", ")} )${senderPart}"""

    val post = {
      val raw = {
        if (!constantSection) {
          if (async) "stub.TransactionInfo.Async" else "stub.TransactionInfo"
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
