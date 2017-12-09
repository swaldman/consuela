package com.mchange.sc.v1.consuela.ethereum.stub

import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi

import scala.collection._

import java.io.StringWriter

import com.mchange.sc.v2.lang.borrow

import com.mchange.v2.io.IndentedWriter

object Generator {

  private val StubImports = immutable.Seq(
    "scala.collection._",
    "scala.concurrent._",
    "scala.concurrent.duration.Duration",
    "com.mchange.sc.v2.concurrent.Poller",
    "com.mchange.sc.v1.consuela._",
    "com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthLogEntry}",
    "com.mchange.sc.v1.consuela.ethereum.ethabi",
    "com.mchange.sc.v1.consuela.ethereum.ethabi.SolidityEvent",
    "com.mchange.sc.v1.consuela.ethereum.stub._",
    "com.mchange.sc.v1.consuela.ethereum.stub.{Event => GenericEvent}",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc",
    "com.mchange.sc.v1.consuela.ethereum.jsonrpc.Abi"
  )

  private val AnonymousEventName = "Anonymous"

  private def fillArgs( inputs : immutable.Seq[Abi.Function.Parameter] ) : immutable.Seq[Abi.Function.Parameter] = {
    inputs.zip( Stream.from(1) ).map { case ( param, index ) =>
      if ( param.name.length > 0 ) param else Abi.Function.Parameter( s"arg$index", param.`type` )
    }
  }
  private def fillInputs( fcn : Abi.Function ) = fcn.copy( inputs = fillArgs( fcn.inputs ) )

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

    borrow( new IndentedWriter( sw ) ) { iw =>
      iw.println( s"package ${fullyQualifiedPackageName}" )
      iw.println()
      StubImports.foreach { imported =>
        iw.println( s"import $imported" )
      }
      iw.println()


      iw.println( s"final object $className {" )
      iw.upIndent()
      iw.println( "sealed trait Event extends GenericEvent" )
      iw.println()
      writeAllEventDefinitions( AnonymousEventName, className, abi, iw )
      iw.downIndent()
      iw.println(  "}" )
      iw.println( s"final case class $className( val contractAddress : EthAddress )( implicit icontext : jsonrpc.Invoker.Context, cfactory : jsonrpc.Client.Factory, poller : Poller, econtext : ExecutionContext ) {" )
      iw.upIndent()
      iw.println( s"final object transaction {" )
      iw.upIndent()
      abi.functions.foreach { fcn =>
        writeFunction( fillInputs(fcn), false, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
      iw.println( s"final object constant {" )
      iw.upIndent()
      abi.functions.filter( _.constant ).foreach { fcn =>
        writeFunction( fillInputs(fcn), true, async, iw )
        iw.println()
      }
      iw.downIndent()
      iw.println( "}" )
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

  /*
   * If there is just one anonymous event type defined, we know its type signature,
   * so we can define it as a typed event.
   * 
   * If there are multiple anonymous events defined, we can't easily know at runtime
   * which anonymous event we are encountering, so we defined a generic event wrapping
   * the raw log entry
   */ 
  private def writeAllEventDefinitions( anonEventName : String, stubClassName : String, abi : Abi, iw : IndentedWriter ) : Unit = {
    val ( named, anonymous ) = abi.events.partition( _.anonymous == false )
    named foreach { event =>
      val resolvedEventName = abiEventToResolvedName( event, abi )
      writeTypedEventDefinition( event, resolvedEventName, stubClassName, iw )
    }
    anonymous.length match {
      case 1 => writeTypedAnonymousEventDefinition( anonymous(0), anonEventName, stubClassName, iw )
      case _ => anonymous foreach { event =>
        writeUntypedAnonymousEventDefinition( anonEventName, stubClassName, iw )
      }
    }
  }

  private def writeUntypedAnonymousEventDefinition (
    anonEventName : String,
    stubClassName : String,
    iw            : IndentedWriter
  ) : Unit = {
    iw.println( s"final object ${ anonEventName } {" )
    iw.upIndent()
    iw.println( "def apply( solidityEvent : SolidityEvent.Anonymous, metadata : GenericEvent.Metadata ) : ${ anonEventName } = {" )
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
    iw.println( s"final case class ${anonEventName} (" )
    iw.upIndent()
    iw.println( "metadata : GenericEvent.Metadata," )
    iw.println( "logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubClassName}.Event" )
    iw.println()
  }

  private def writeTypedAnonymousEventDefinition (
    event         : Abi.Event,
    anonEventName : String,
    stubClassName : String,
    iw            : IndentedWriter
  ) : Unit = {
    writeTypedEventDefinition( event, anonEventName, stubClassName, iw ) 
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
    iw.println( s"def apply( solidityEvent : SolidityEvent.${solidityEventType}, metadata : GenericEvent.Metadata ) : ${ resolvedEventName } = {" )
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
    iw.println( "metadata : GenericEvent.Metadata," )
    iw.println( "logEntry : EthLogEntry" )
    iw.downIndent()
    iw.println( s") extends ${stubClassName}.Event" )
    iw.println()
  }

  private def writeFunction( fcn : Abi.Function, constantSection : Boolean, async : Boolean, iw : IndentedWriter ) : Unit = {
    def toRepLambda( param : Abi.Function.Parameter ) : String = {
      val tpe = param.`type`
      val helper = forceHelper( tpe )
      helper.inConversionGen( param.name )
    }
    def abiFunctionCtor = {
      def paramctor( param : Abi.Function.Parameter ) = s"""Abi.Function.Parameter( "${param.name}", s"${param.`type`}" )"""
      def paramctors( seq : immutable.Seq[Abi.Function.Parameter] ) = seq.map( paramctor ).mkString( ", " )
      s"""|Abi.Function( 
          |  name = "${fcn.name}",
          |  inputs = immutable.Seq( ${paramctors(fcn.inputs)} ),
          |  outputs = immutable.Seq( ${paramctors(fcn.outputs)} ),
          |  constant = ${fcn.constant},
          |  payable = ${fcn.payable},
          |  stateMutability = "${fcn.stateMutability}" 
          |)""".stripMargin
    }

    iw.println( functionSignature( fcn, constantSection, async ) + " = {" )
    iw.upIndent()

    if (! fcn.payable) {
      iw.println("val optionalPaymentInWei : Option[sol.UInt256] = None")
      iw.println()
    }
    iw.println( s"""val fcn = ${abiFunctionCtor}""" )
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
      iw.println( s"""val futTransactionInfo = futHash.flatMap( hash => jsonrpc.Invoker.futureTransactionReceipt( hash ).map( mbctr => TransactionInfo.Message.fromJsonrpcReceipt( hash, mbctr ) ) )""" )
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
    
    val prereturn = s"""def ${fcn.name}( ${params.mkString(", ")} )( implicit sender : Sender )"""

    val post = {
      val raw = {
        if (!constantSection) {
          "TransactionInfo.Message"
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
