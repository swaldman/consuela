package com.mchange.sc.v1.consuela.ethereum.jsonrpc

import scala.annotation.tailrec
import scala.collection._
import play.api.libs.json._

final object LooseAbi {
  import Ordering.Implicits._
  implicit val Ordering_FunctionParameter    = Ordering.by( (fp : Function.Parameter)    => (fp.name, fp.`type`, fp.internalType)                                               )
  implicit val Ordering_ConstructorParameter = Ordering.by( (cp : Constructor.Parameter) => (cp.name, cp.`type`, cp.internalType)                                               )
  implicit val Ordering_EventParameter       = Ordering.by( (ep : Event.Parameter)       => (ep.name, ep.`type`, ep.internalType, ep.indexed)                                   )
  implicit val Ordering_Function             = Ordering.by( (fcn : Function)             => (fcn.name, fcn.inputs, fcn.outputs, fcn.constant, fcn.payable, fcn.stateMutability) )
  implicit val Ordering_Constructor          = Ordering.by( (ctor : Constructor)         => (ctor.inputs, ctor.payable, ctor.stateMutability)                                   )
  implicit val Ordering_Event                = Ordering.by( (ev : Event)                 => (ev.name, ev.inputs, ev.anonymous)                                                  )

  def apply( json : JsValue ) : LooseAbi = {
    json match {
      case jsa : JsArray => apply( jsa )
      case _             => throw new BadAbiException( s"An ABI must be a JSON vector/array, found: ${json}" )
    }
  }
  def apply( json : String ) : LooseAbi = apply( Json.parse( json ) )

  lazy val empty : LooseAbi = LooseAbi( JsArray.empty )

  private final def requireRetrieve( json : JsObject, key : String, in : String ) : JsValue = json.value.getOrElse( key, throw new BadAbiException( s"No 'name' field in ${in} JSON: ${json}" ) )

  final object Function {
    case class Parameter( json : JsObject ) {
      val name         : String         = requireRetrieve( json, "name", "function parameter" ).as[String]
      val `type`       : String         = requireRetrieve( json, "type", "function parameter" ).as[String]
      val internalType : Option[String] = json.value.get( "internalType" ).map( _.as[String] )

      lazy val sorted = Function.Parameter( JsObject( SortedMap.empty[String,JsValue] ++ json.value ) )
    }
  }
  final case class Function( json : JsObject ) {
    val name            : String                            = requireRetrieve( json, "name", "function" ).as[String]
    val inputs          : immutable.Seq[Function.Parameter] = requireRetrieve( json, "inputs", "function" ).as[JsArray].value.map( jsv => Function.Parameter( jsv.as[JsObject] ) ).toList
    val outputs         : immutable.Seq[Function.Parameter] = requireRetrieve( json, "outputs", "function" ).as[JsArray].value.map( jsv => Function.Parameter( jsv.as[JsObject] ) ).toList

    private val _constant        : Option[Boolean]                   = json.value.get( "constant" ).map( _.as[Boolean] )
    private val _payable         : Option[Boolean]                   = json.value.get( "payable" ).map( _.as[Boolean] )
    private val _stateMutability : Option[String]                    = json.value.get( "stateMutability" ).map( _.as[String] )

    val ( constant, payable, stateMutability ) = resolveStateMutabilities

    def resolveStateMutabilities : (Boolean, Boolean, Option[String] ) = {
      ( _constant, _payable, _stateMutability ) match {
        case ( Some( c ), Some( p ), ssm @ Some( sm ) ) => {
          sm match {
            case "pure" | "view" => {
              if (!c) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with constant value '${c}' in function: ${json}" )
              if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
            }
            case "payable" => {
              if (!p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
            }
            case "nonpayable" => {
              if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
            }
            case other => {
              throw new BadAbiException( s"Unexpected state mutability value '${sm}' in function: ${json}" )
            }
          }
          ( c, p, ssm )
        }
        case ( Some( c ), Some( p ), None ) => {
          if (c && p) throw new BadAbiException( s"Inconsistent ABI, function cannot be both constant and payable: ${json}" )
          ( c, p, None )
        }
        case ( Some ( c ), None, None ) => { // early ABI, every nonconstant function payable
          ( c, !c, None )
        }
        case ( None, sp @ Some( p ), None ) => {
          if ( !p ) throw new BadAbiException( s"Incomplete function ABI, cannot even deterine whether state mutability is constant: ${json}" )
          ( false, p, None )
        }
        case ( None, None, ssm @ Some( sm ) ) => {
          sm match {
            case "pure" | "view" => {
              ( true, false, ssm )
            }
            case "payable" => {
              ( false, true, ssm )
            }
            case "nonpayable" => {
              ( false, false, ssm )
            }
            case other => {
              throw new BadAbiException( s"Unexpected state mutability value '${sm}' in function: ${json}" )
            }
          }
        }
        case ( Some( c ), None, ssm @ Some( sm ) ) => {
          sm match {
            case "pure" | "view" => {
              if (!c) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with constant value '${c}' in function: ${json}" )
              ( c, false, ssm )
            }
            case "payable" => {
              if (c) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with constant value '${c}' in function: ${json}" )
              ( c, true, ssm )
            }
            case "nonpayable" => {
              if (c) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with constant value '${c}' in function: ${json}" )
              ( c, false, ssm )
            }
            case other => {
              throw new BadAbiException( s"Unexpected state mutability value '${sm}' in function: ${json}" )
            }
          }
        }
        case ( None, Some( p ), ssm @ Some( sm ) ) => {
          sm match {
            case "pure" | "view" => {
              if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
              ( true, p, ssm )
            }
            case "payable" => {
              if (!p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
              ( false, p, ssm )
            }
            case "nonpayable" => {
              if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in function: ${json}" )
              ( false, p, ssm )
            }
            case other => {
              throw new BadAbiException( s"Unexpected state mutability value '${sm}' in function: ${json}" )
            }
          }
        }
        case ( None, None, None ) => {
          throw new BadAbiException( s"No information about constancy or state mutability in function: ${json}" )
        }
      }
    }


    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      val sortedOutputs = JsArray( outputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) + ("outputs" -> sortedOutputs) ).toSeq ) )
    }
  }
  final object Constructor {
    val noArgNoEffect : Constructor = this.apply( JsObject( "inputs" -> JsArray.empty :: "stateMutability" -> JsString("nonpayable") :: Nil ) )
    final case class Parameter( json : JsObject ) {
      val name         : String         = requireRetrieve( json, "name", "constructor parameter" ).as[String]
      val `type`       : String         = requireRetrieve( json, "type", "constructor parameter" ).as[String]
      val internalType : Option[String] = json.value.get( "internalType" ).map( _.as[String] )

      lazy val sorted = Constructor.Parameter( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
    }
  }
  final case class Constructor( json : JsObject ) {
    val inputs          : immutable.Seq[Constructor.Parameter] = requireRetrieve( json, "inputs", "constructor" ).as[JsArray].value.map( jsv => Constructor.Parameter( jsv.as[JsObject] ) ).toList

    private val _payable         : Option[Boolean] = json.value.get("payable").map( _.as[Boolean] )
    private val _stateMutability : Option[String]  = json.value.get("stateMutability").map( _.as[String] )

    val ( payable, stateMutability ) = resolvePayableStateMutability( json,  "constructor", _payable, _stateMutability )

    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) ).toSeq ) )
    }
  }
  final object Event {
    final case class Parameter( json : JsObject ) {
      val name         : String         = requireRetrieve( json, "name", "event parameter" ).as[String]
      val `type`       : String         = requireRetrieve( json, "type", "event parameter" ).as[String]
      val indexed      : Boolean        = json.value.get("indexed").map( _.as[Boolean] ).getOrElse( false )
      val internalType : Option[String] = json.value.get("internalType").map( _.as[String] )

      lazy val sorted = Event.Parameter( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
    }
  }
  final case class Event( json : JsObject ) {
    val name      : String                         = requireRetrieve( json, "name", "event" ).as[String]
    val inputs    : immutable.Seq[Event.Parameter] = requireRetrieve( json, "inputs", "event" ).as[JsArray].value.map( jsv => Event.Parameter( jsv.as[JsObject] ) ).toList
    val anonymous : Boolean                        = json.value.get("anonymous").map( _.as[Boolean] ).getOrElse(false) // defaults to false because very old ABIs omitted, anonymous events had not yet been defined 

    lazy val sorted = {
      val sortedInputs  = JsArray( inputs.map( _.sorted ).sorted.map( _.json ) )
      Function( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value + ("inputs" -> sortedInputs) ).toSeq ) )
    }
  }
  final case class Receive( json : JsObject ) {
    val stateMutability : Option[String] = json.value.get("stateMutability").map( _.as[String] )

    lazy val sorted = Receive( JsObject( ( SortedMap.empty[String,JsValue] ++ json.value ).toSeq ) )
  }
  final case class Fallback( json : JsObject ) {
    private val _payable         : Option[Boolean] = json.value.get("payable").map( _.as[Boolean] )
    private val _stateMutability : Option[String]  = json.value.get("stateMutability").map( _.as[String] )

    lazy val ( payable, stateMutability ) = resolvePayableStateMutability( json, "fallback",  _payable, _stateMutability )
  }

  private def resolvePayableStateMutability( json : JsObject, in : String,  _payable : Option[Boolean], _stateMutability : Option[String] ) : ( Boolean, Option[String] ) = {
    ( _payable, _stateMutability ) match {
      case ( Some( p ), ssm @ Some( sm ) ) => {
        sm match {
          case "pure" | "view" => {
            if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in ${in}: ${json}" )
            ( p, ssm )
          }
          case "payable" => {
            if (!p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in ${in}: ${json}" )
            ( p, ssm )
          }
          case "nonpayable" => {
            if (p) throw new BadAbiException( s"State mutability '${sm}' is inconsistent with payable value '${p}' in ${in}: ${json}" )
            ( p, ssm )
          }
          case other => {
            throw new BadAbiException( s"Unexpected state mutability value '${sm}' in ${in}: ${json}" )
          }
        }
      }
      case ( None, ssm @ Some( sm ) ) => {
        sm match {
          case "pure" | "view" | "nonpayable" => {
            ( false, ssm )
          }
          case "payable" => {
            ( true, ssm )
          }
          case other => {
            throw new BadAbiException( s"Unexpected state mutability value '${sm}' in ${in}: ${json}" )
          }
        }
      }
      case ( Some( p ), None ) => {
        ( p, None )
      }
      case ( None, None ) => { // very old ABIs, everything considered payable
        ( true, None )
      }
    }
  }
}
case class LooseAbi( json : JsArray ) extends MaybeEmpty {

  @tailrec
  private final def segregateByType(
    src : IndexedSeq[JsValue],
    i   : Int,
    f : List[JsObject],
    e   : List[JsObject],
    c   : List[JsObject],
    r   : List[JsObject],
    fb  : List[JsObject],
    u   : List[JsValue]
  ) : Tuple6[List[JsObject],List[JsObject],List[JsObject],List[JsObject],List[JsObject],List[JsValue]] = {
    if (i == src.length) {
      ( f, e, c, r, fb, u )
    }
    else {
      src(i) match {
        case jso : JsObject => {
          jso.value.get("type").map( _.as[String] ) match {
            case Some( "function"    ) => segregateByType( src, i+1, jso :: f, e, c, r, fb, u )
            case Some( "event"       ) => segregateByType( src, i+1, f, jso :: e, c, r, fb, u )
            case Some( "constructor" ) => segregateByType( src, i+1, f, e, jso :: c, r, fb, u )
            case Some( "receive"     ) => segregateByType( src, i+1, f, e, c, jso :: r, fb, u )
            case Some( "fallback"    ) => segregateByType( src, i+1, f, e, c, r, jso :: fb, u )
            case _                     => segregateByType( src, i+1, f, e, c, r, fb, jso :: u )
          }
        }
        case other => {
          segregateByType( src, i+1, f, e, c, r, fb, other :: u )
        }
      }
    }
  }

  val ( functions, events, constructors, receive, fallback, unexpected ) = {
    import LooseAbi._
    val ( f, e, c, r, fb, u ) = segregateByType( json.value, 0, Nil, Nil, Nil, Nil, Nil, Nil )
    if ( r.size > 1  ) throw new BadAbiException( s"Only one 'receive' element permitted, ${r.size} found: ${JsArray(r.toVector)}"    )
    if ( fb.size > 1 ) throw new BadAbiException( s"Only one 'fallback' element permitted, ${fb.size} found: ${JsArray(fb.toVector)}" )

    def opt[T]( l : List[T] ) : Option[T] = {
      l match {
        case Nil         => None
        case elem :: Nil => Some( elem )
        case other       => throw new AssertionError( s"List contains more than one element: ${l}" )
      }
    }

    ( f.map( Function.apply ), e.map( Event.apply ), c.map( Constructor.apply ), opt( r.map( Receive.apply ) ), opt( fb.map( Fallback.apply ) ), u.toIndexedSeq )
  }

  lazy val sorted = {
    LooseAbi(
      JsArray(
        immutable.IndexedSeq.empty[JsValue]                 ++
          functions.map( _.sorted ).sorted.map( _.json )    ++
          events.map( _.sorted).sorted.map( _.json )        ++
          constructors.map( _.sorted ).sorted.map( _.json ) ++
          receive.map( _.json )                             ++
          fallback.map( _.json )                            ++
          unexpected
      )
    )
  }

  def isEmpty : Boolean = json.value.isEmpty
  def withStandardSort : LooseAbi = sorted
}
