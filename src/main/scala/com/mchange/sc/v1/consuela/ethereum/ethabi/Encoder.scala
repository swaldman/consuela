package com.mchange.sc.v1.consuela.ethereum.ethabi

import scala.collection._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.math._

import com.mchange.sc.v1.consuela.ethereum.EthAddress
import com.mchange.sc.v1.consuela.ethereum.specification.Types.ByteSeqExact20

import com.mchange.sc.v2.failable._

import com.mchange.lang.ByteUtils.unsignedPromote

import scala.language.existentials

import scala.annotation.tailrec

object Encoder {
  private def allZero( bytes : Seq[Byte] ) : Boolean = ! bytes.exists( _ != 0 )

  private val ZeroByte = 0.toByte
  private val OneByte  = 1.toByte

  private val TwoBigInt     = BigInt(2)
  private val TwoBigDecimal = BigDecimal(2)

  private val Mappables : Map[String, Encoder[_]] = {
    val fixedLenByteArrayBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( len => s"bytes${len}" -> new Encoder.PredefinedByteArray(len) )

    val byteBinding : Tuple2[String,Encoder[_]] = ( "byte", fixedLenByteArrayBindings(0)._2 )
    val boolBinding : Tuple2[String,Encoder[_]] = ( "bool", Encoder.Bool )

    val uintBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( _ * 8 ).map( bitLen => s"uint${bitLen}" -> new Encoder.UInt( bitLen ) )
    val  intBindings : Seq[Tuple2[String,Encoder[_]]] = (1 to 32).map( _ * 8 ).map( bitLen => s"int${bitLen}" -> new Encoder.SInt( bitLen ) )

    val defaultUIntBinding : Tuple2[String,Encoder[_]] = "uint" -> uintBindings.last._2
    val  defaultIntBinding : Tuple2[String,Encoder[_]] = "int"  -> intBindings.last._2

    val addressBinding = "address" -> Encoder.Address

    val allBindings : List[Tuple2[String,Encoder[_]]] = {
      fixedLenByteArrayBindings.toList ::: uintBindings.toList ::: intBindings.toList ::: byteBinding :: boolBinding :: defaultUIntBinding :: defaultIntBinding :: addressBinding :: Nil
    }
    Map( allBindings : _* )
  }

  private lazy val UInt160 = Mappables("uint160").asInstanceOf[Encoder[BigInt]]

  lazy val UInt256 = Mappables("uint256").asInstanceOf[Encoder[BigInt]]
  lazy val  Int256 = Mappables("int256").asInstanceOf[Encoder[BigInt]]

  private def xfixed( m : Int, n : Int, internal : Encoder[BigInt], signed : Boolean ) : Encoder[BigDecimal] = {
    new Encoder[BigDecimal] {
      require( m + n <= 256 )

      val denominator = TwoBigDecimal.pow(n)

      def notRepresentableMessage( bd : BigDecimal ) = s"${bd} cannot be represented as a multiple of 1/(2 ** ${n})"
      def downToBigDecimal( bi : BigInt ) = Failable( BigDecimal( bi ) / TwoBigDecimal.pow(n) )
      def upToBigInt( bd : BigDecimal )   = Failable( bd * denominator ).withFilter( _.isWhole ).map( _.toBigInt ).xmap( _ => Fail.simple( notRepresentableMessage( bd ) ) )
      def isRepresentable( bd : BigDecimal ) = (bd * denominator).isWhole
      def representableOrFail( bd : BigDecimal ) = isRepresentable( bd ).toFailable( notRepresentableMessage( bd ) ).map( _ => bd )

      def parse( str : String ) : Failable[BigDecimal] = {
        for {
          raw <- Failable( BigDecimal( str ) )
          _ <- ( signed || raw >= 0 ).toFailable("Unsigned parser can't parse values below zero.")
          result <- representableOrFail( raw )
        } yield {
          result
        }
      }
      def format( representation : BigDecimal ) : Failable[String] = {
       representableOrFail( representation ).map( _.toString )
      }
      def encode( representation : BigDecimal ) : Failable[immutable.Seq[Byte]] = {
        upToBigInt( representation ).flatMap( internal.encode )
      }
      def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[BigDecimal,immutable.Seq[Byte]]] = {
        internal.decode( bytes ).flatMap { case ( bi, rest ) => downToBigDecimal( bi ).map{ bd => ( bd, rest ) } }
      }
      def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[BigDecimal] = {
        internal.decodeComplete( bytes ).flatMap( downToBigDecimal )
      }
      def encodingLength : Option[Int] = {
        internal.encodingLength
      }
    }
  }

  private def  fixed( m : Int, n : Int ) = xfixed( m, n,  Int256, true )
  private def ufixed( m : Int, n : Int ) = xfixed( m, n, UInt256, false )

  private val  FixedRegex =  """fixed(\d{1,3})x(\d{1,3})""".r
  private val UFixedRegex = """ufixed(\d{1,3})x(\d{1,3})""".r

  private val OuterArrayRegex = """^(.*)\[\]$""".r
  private val InnerArrayRegex = """^(.*)\[\(d+)\]$""".r

  def encoderForSolidityType( typeName : String ) : Option[Encoder[_]] = {
    def resolveFixedType : Option[Encoder[_]] = {
      val fullyQualifiedName = typeName match {
        case  "fixed" =>  "fixed128x128"
        case "ufixed" => "ufixed128x128"
        case other    => other
      }
      fullyQualifiedName match {
        case  FixedRegex( m, n ) => Some(  fixed( m.toInt, n.toInt ) )
        case UFixedRegex( m, n ) => Some( ufixed( m.toInt, n.toInt ) )
        case _                   => None
      }
    }
    def resolveDynamicType : Option[Encoder[_]] = {
      val unspaced = typeName.filter( c => !c.isWhitespace )
      unspaced match {
        case InnerArrayRegex( elementTypeName, length ) => Some( new InnerArrayDecoder( elementTypeName, length.toInt ) )
        case OuterArrayRegex( elementTypeName )         => Some( new OuterArrayDecoder( elementTypeName ) )
        case _                                          => None
      }
    }

    Mappables.get( typeName ) orElse resolveFixedType orElse resolveDynamicType
  }

  case class ArrayRep( elementTypeName : String, items : immutable.Seq[Any] )

  private trait EscapeState
  private case object AfterSlash extends EscapeState
  private case object AtHex      extends EscapeState
  private case object AtUnicode  extends EscapeState

  private trait QuoteState
  private case object NoQuote                                                            extends QuoteState
  private case object InQuote                                                            extends QuoteState

  // escapeIndex is only used for digits of hex and unicode escapes
  private case class  InQuoteEscape( escapeState : EscapeState, escapeIndex : Int = -1 ) extends QuoteState

  private val SingleCharEscapeAndHexChars  = Set('a','b','f')
  private val NoEscapeHexChars             = Set('0','1','2','3','4','5','6','7','8','9','c','d','e','A','B','C','D','E','F')
  private val SingleCharEscapesNotHexChars = Set('n','r','t','v','\\','\'','\"','?')

  private def parseArray( elementTypeName : String, inner : Encoder[_] )( str : String ) : Failable[ArrayRep] = {

    val strim    = str.trim
    val strimLen = strim.length

    if ( strim.length < 2 ) return fail( "A String representation of an array must have at least two characters, [ and ]!" )
    if ( strim.charAt(0) != '[' || strim.charAt( strimLen - 1 ) != ']' ) return fail( "A String representation of an array must begin with '[' and end with ']'!" )

    val uncapped = strim.substring( 1, strimLen - 1 )
    val len = uncapped.length

    /*
     * Note: We'll use Exceptions for parse errors in findTopCommas
     */

    @tailrec
    def findTopCommas( index : Int, arrayLevel : Int, quoteState : QuoteState, found : List[Int] ) : List[Int] = {
      def invalidEscapeCharQuoteTransition( c : Char ) : QuoteState = {
        quoteState match {
          case NoQuote                                   => NoQuote
          case InQuote                                   => InQuote
          case InQuoteEscape( escapeState, escapeIndex ) => throw new Exception( s"""'${c}' invalid in String escape. [At character ${index} of "${uncapped}"]""" )
        }
      }
      if ( index < len ) {
        uncapped.charAt( index ) match {
          case ',' => {
            val newFound = if ( arrayLevel == 0 && quoteState == NoQuote ) index :: found else found
            val newQuoteState = invalidEscapeCharQuoteTransition(',')
            findTopCommas( index + 1, arrayLevel, newQuoteState, newFound )
          }
          case '[' => {
            val newArrayLevel = if ( quoteState == NoQuote ) arrayLevel + 1 else arrayLevel
            val newQuoteState = invalidEscapeCharQuoteTransition('[')
            findTopCommas( index + 1, newArrayLevel, newQuoteState, found )
          }
          case ']' => {
            val newArrayLevel = if ( quoteState == NoQuote ) arrayLevel - 1 else arrayLevel
            val newQuoteState = invalidEscapeCharQuoteTransition(']')
            findTopCommas( index + 1, newArrayLevel, newQuoteState, found )
          }
          case '"' => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => InQuote
                case InQuote                         => NoQuote
                case InQuoteEscape( AfterSlash, -1 ) => InQuote
                case InQuoteEscape( _, _ )           => throw new Exception( s"""'"' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case 'x' => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => NoQuote
                case InQuote                         => InQuote
                case InQuoteEscape( AfterSlash, -1 ) => InQuoteEscape( AtHex, 0 )
                case InQuoteEscape( _, _ )           => throw new Exception( s"""'x' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case 'u' => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => NoQuote
                case InQuote                         => InQuote
                case InQuoteEscape( AfterSlash, -1 ) => InQuoteEscape( AtUnicode, 0 )
                case InQuoteEscape( _, _ )           => throw new Exception( s"""'x' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case c if SingleCharEscapeAndHexChars(c) => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => NoQuote
                case InQuote                         => InQuote
                case InQuoteEscape( AfterSlash, -1 ) => InQuote
                case InQuoteEscape( AtHex, i )       => {
                  i match {
                    case 0 => InQuoteEscape( AtHex, 1 )
                    case 1 => InQuote
                  }
                }
                case InQuoteEscape( AtUnicode, i ) => {
                  i match {
                    case 0|1|2 => InQuoteEscape( AtUnicode, i+1 )
                    case 3     => InQuote
                  }
                }
                case InQuoteEscape( _, _ ) => throw new Exception( s"""'${c}' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case c if NoEscapeHexChars(c) => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => NoQuote
                case InQuote                         => InQuote
                case InQuoteEscape( AtHex, i )       => {
                  i match {
                    case 0 => InQuoteEscape( AtHex, 1 )
                    case 1 => InQuote
                  }
                }
                case InQuoteEscape( AtUnicode, i ) => {
                  i match {
                    case 0|1|2 => InQuoteEscape( AtUnicode, i+1 )
                    case 3     => InQuote
                  }
                }
                case InQuoteEscape( _, _ ) => throw new Exception( s"""'${c}' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case c if SingleCharEscapesNotHexChars(c) => {
            val newQuoteState = {
              quoteState match {
                case NoQuote                         => NoQuote
                case InQuote                         => InQuote
                case InQuoteEscape( AfterSlash, -1 ) => InQuote
                case InQuoteEscape( _, _ ) => throw new Exception( s"""'${c}' at invalid position in String escape. [At character ${index} of "${uncapped}"]""" )
              }
            }
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
          case c => {
            val newQuoteState = invalidEscapeCharQuoteTransition('[')
            findTopCommas( index + 1, arrayLevel, newQuoteState, found )
          }
        }
      } else {
        found
      }
    }

    val topCommas = Failable( findTopCommas( 0, 0, NoQuote, Nil ) )

    val elements = {
      topCommas.map { tcs =>
        val capped  = -1 :: tcs ::: -1 :: Nil
        val grouped = capped.sliding(2)
        grouped.map {
          case         -1 :: endComma :: Nil                                          => uncapped.substring( endComma ).trim
          case startComma ::       -1 :: Nil if ( startComma + 1 == uncapped.length ) => ""
          case startComma ::       -1 :: Nil                                          => uncapped.substring( startComma + 1 ).trim
          case startComma :: endComma :: Nil                                          => uncapped.substring( startComma + 1 , endComma ).trim
        }
      }
    }

    elements.flatMap { list => Failable( ArrayRep( elementTypeName, list.map( inner.parse ).map( _.get ).toVector ) ) }
  }
  private def formatArray( inner : Encoder[_] )( representation : ArrayRep ) : Failable[String] = {
    Failable.sequence( representation.items.map( inner.formatAny ) ).map( _.mkString("[",",","]") )
  }

  final class OuterArrayDecoder( elementTypeName : String ) extends Encoder[ArrayRep] {

    private def checkElementType( representation : ArrayRep ) : Failable[Boolean] = {
      ( representation.elementTypeName == elementTypeName ).toFailable( s"A decoder of elements of type '${elementTypeName}' can't decode elements of type '${representation.elementTypeName}'." )
    }
    def parse( str : String ) : Failable[ArrayRep] = {
      val innerEncoder = new InnerArrayDecoder( elementTypeName )
      parseArray( elementTypeName, innerEncoder )( str )
    }
    def format( representation : ArrayRep ) : Failable[String] = {
      checkElementType( representation ).flatMap { _ =>
        val innerEncoder = new InnerArrayDecoder( representation.elementTypeName )
        formatArray( innerEncoder )( representation )
      }
    }

    def encode( representation : ArrayRep )   : Failable[immutable.Seq[Byte]] = {
      checkElementType( representation ).flatMap { _ =>
        val elementCount = representation.items.size
        val innerEncoder = new InnerArrayDecoder( elementTypeName, elementCount )

        for {
          prefix    <- UInt256.encode( elementCount )
          arrayBody <- innerEncoder.encode( representation )
        } yield {
          prefix ++ arrayBody
        }
      }
    }
    def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[ArrayRep,immutable.Seq[Byte]]] = {
      UInt256.decode( bytes ).flatMap { case ( size, rest ) =>
        size.isValidInt.toFailable( s"Size ${size} is too big to be represented as an Int!" ).flatMap( _ => (new InnerArrayDecoder( elementTypeName, size.toInt )).decode( rest ) )
      }
    }
    def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[ArrayRep] = {
      for {
        ( rep, next ) <- decode( bytes )
        _ <- (next.isEmpty).toFailable( s"Unconsumed bytes in decodeComplete: ${next.hex}" )
      } yield {
        rep
      }
    }
    def encodingLength : Option[Int] = None // will be None, signifying unknown, for dynamic types
  }

  final class InnerArrayDecoder( val elementTypeName : String, val elementCount : Int = -1 ) extends Encoder[ArrayRep] {

  //val check = (list.length == elementCount).toFailable( s"Unexpected array size. [expected: ${elementCount}, found: ${list.length}]" )

    val inner = encoderForSolidityType( elementTypeName ).get // asserts availability of elementTypeName

    def parse( str : String )               : Failable[ArrayRep] = {
      for {
        arep <- parseArray( elementTypeName, inner )( str )
        _    <- (arep.items.length == elementCount).toFailable( s"Unexpected array size. [expected: ${elementCount}, found: ${arep.items.length}]" )
      } yield {
        arep
      }
    }

    def format( representation : ArrayRep ) : Failable[String] = formatArray( inner )( representation )

    def encode( representation : ArrayRep ) : Failable[immutable.Seq[Byte]] = {
      Failable( representation.items.flatMap( item => inner.encodeAny( item ).get ) )
    }
    def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[ArrayRep,immutable.Seq[Byte]]] = {
      val check = ( elementCount >= 0).toFailable("We need to know the element count to decode an array!")
      check.flatMap( _ => _decode( elementCount, elementCount, Array.ofDim[Any]( elementCount ), bytes ) )
    }

    @tailrec
    private def _decode( countdown : Int, len : Int, nascent : Array[Any], more : immutable.Seq[Byte] ) : Failable[Tuple2[ArrayRep,immutable.Seq[Byte]]] = {
      if ( countdown == 0 ) {
        succeed( Tuple2( ArrayRep( elementTypeName, nascent.toVector ), more ) )
      } else {
        // convoluted to keep this function tail recursive
        // note the crucial side effect
        val goodBytes = inner.decode( more ).map { case ( nextValue, nextBytes ) =>
          nascent( len - countdown ) = nextValue
          nextBytes
        }
        if ( goodBytes.isFailed ) {
          refail( goodBytes.asFailed )
        } else {
          _decode( countdown - 1, len, nascent, goodBytes.get )
        }
      }
    }
    def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[ArrayRep] = {
      for {
        ( out, rest ) <- decode( bytes )
        _             <- (rest.isEmpty).toFailable( s"decodeComplete(...) expects to decode an exact byte representation, and yet we found bytes left over: 0x${rest.hex}" )
      } yield {
        out
      }
    }
    def encodingLength : Option[Int] = None
  }

  final object Bool extends FixedLengthRepresentation[Boolean]( 32 ) {
    def parse( str : String ) : Failable[Boolean] = {
      Failable( java.lang.Boolean.parseBoolean( str ) )
    }
    def format( representation : Boolean ) : Failable[String] = {
      Failable( String.valueOf( representation ) )
    }
    def encode( representation : Boolean ) : Failable[immutable.Seq[Byte]] = {
      succeed( (0 until 31).map( _ => ZeroByte ) :+ (if ( representation ) OneByte else ZeroByte) )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : immutable.Seq[Byte] ) : Failable[Boolean] = {
      for {
        _ <- allZero( bytes.init ).toFailable( s"All but the last byte of an encoded bool should be zero! ${bytes.hex}" )
        _ <- ( bytes.tail == 0 || bytes.tail == 1 ).toFailable("The last byte of encoded bool should be 0 or 1.")
      } yield {
        if ( bytes.tail == 0 ) true else false
      }
    }
  }
  final object Address extends Encoder[EthAddress] {
    private def toAddress( bi : BigInt )         : EthAddress = EthAddress( ByteSeqExact20( asFixedLengthUnsignedByteArray( bi, 20 ).toImmutableSeq ) )
    private def toBigInt( address : EthAddress ) : BigInt     = BigInt( 1, address.bytes.widen.toArray )

    def parse( str : String ) : Failable[EthAddress]             = Failable( EthAddress( str ) )
    def format( representation : EthAddress ) : Failable[String] = Failable( "0x"+representation.hex )

    def encode( representation : EthAddress ) : Failable[immutable.Seq[Byte]]                 = UInt160.encode( toBigInt( representation ) )
    def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[EthAddress,immutable.Seq[Byte]]] = {
      UInt160.decode( bytes ).map { case ( bi, rest ) => ( toAddress(bi), rest ) }
    }
    def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[EthAddress] = UInt160.decodeComplete( bytes ).map( toAddress )

    def encodingLength : Option[Int] = UInt160.encodingLength
  }
  final class UInt( bitLen : Int ) extends FixedLengthRepresentation[BigInt]( 32 ) {
    require( bitLen % 8 == 0 )

    val byteLen = bitLen / 8

    private def checkRange( v : BigInt ) : Failable[Boolean] = (v >= 0 && v < ceiling ).toFailable( s"uint${bitLen} must be greater than or equal to zero and less than ${ceiling}, but is ${v}" )

    val ceiling = TwoBigInt.pow( bitLen )

    def parse( str : String ) : Failable[BigInt] = {
      val v = BigInt( str )
      checkRange(v).map( _ => v )
    }
    def format( representation : BigInt ) : Failable[String] = {
      checkRange( representation ).map( _.toString )
    }
    def encode( representation : BigInt ) : Failable[immutable.Seq[Byte]] = {
      checkRange( representation ).map( _ => asFixedLengthUnsignedByteArray( representation, 32 ).toImmutableSeq )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : immutable.Seq[Byte] ) : Failable[BigInt] = {
      val zeroLen = 32 - byteLen
      val ( zeros, good ) = bytes.splitAt( zeroLen )
      val zeroCheck = allZero( zeros ).toFailable( s"We expect a uint${bitLen} to be left-padded with ${zeroLen} zeroes, bytes: ${bytes.hex}" )
      zeroCheck.map( _ => BigInt( 1, good.toArray ) )
    }
  }
  final class SInt( bitLen : Int ) extends FixedLengthRepresentation[BigInt]( 32 ) {
    require( bitLen % 8 == 0 )

    val byteLen = bitLen / 8

    val ceilingExclusive = TwoBigInt.pow( bitLen - 1 )
    val floorInclusive   = -ceilingExclusive

    private def checkRange( v : BigInt ) : Failable[Boolean] = (v >= floorInclusive && v < ceilingExclusive ).toFailable( s"Required ${floorInclusive} <= uint${bitLen} < ${ceilingExclusive}, but is ${v}" )

    def parse( str : String ) : Failable[BigInt] = {
      val v = BigInt( str )
      checkRange(v).map( _ => v )
    }
    def format( representation : BigInt ) : Failable[String] = {
      checkRange( representation ).map( _.toString )
    }
    def encode( representation : BigInt ) : Failable[immutable.Seq[Byte]] = {
      checkRange( representation ).map( _ => asFixedLengthSignedByteArray( representation, 32 ).toImmutableSeq )
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : immutable.Seq[Byte] ) : Failable[BigInt] = {
      val neg = bytes(0) < 0
      val padLen = 32 - byteLen
      val ( pad, good ) = bytes.splitAt( padLen )

      def allNegOne( bytes : Seq[Byte] ) = ! bytes.exists( _ != -1 )

      val padCheck = ( if (neg) allNegOne( pad ) else allZero( pad ) ).toFailable( s"We expect a int${bitLen} to be left-padded with ${padLen} sign-matched bytes, bytes: ${bytes.hex}" )
      padCheck.map( _ => BigInt( good.toArray ) )
    }
  }
  final class PredefinedByteArray( len : Int ) extends FixedLengthRepresentation[immutable.Seq[Byte]]( 32 ) {
    require( len > 0 && len <= 32 )

    private def checkLen( bytes : Seq[Byte] ) : Failable[Boolean] = (bytes.length == len).toFailable( s"Expected ${len} bytes, found ${bytes.length}: ${bytes}")

    def parse( str : String ) : Failable[immutable.Seq[Byte]] = {
      for {
        compact <- Failable( str.filter( c => !c.isWhitespace ) )
        unwrap  <- (str.length >= 2 && str.head == '[' && str.tail == ']').toFailable("An array should start with '[' and end with ']'.").map( _ => compact.substring(1, compact.length-1) )
        split   <- Failable( unwrap.split(",") )
        check   <- (split.length == len).toFailable( s"A predefined byte array of length ${len} should contain exactly ${len} items, but contains ${split.length}: ${split}" )
        bytes   <- Failable.sequence( split.map( b => Failable( b.toByte ) ) )
      } yield {
        bytes
      }
    }
    def format( representation : immutable.Seq[Byte] ) : Failable[String] = {
      checkLen( representation ).map( _ =>  representation.map( unsignedPromote ).mkString("[",",","]") )
    }
    def encode( representation : immutable.Seq[Byte] ) : Failable[immutable.Seq[Byte]] = {
      val padLength = 32 - len
      val padding = (0 until padLength).map( _ => ZeroByte )

      for {
        check  <- checkLen( representation )
        padded <- Failable( representation ++ padding )
      } yield {
        padded
      }
    }
    private [Encoder] def decodeCompleteNoLengthCheck( bytes : immutable.Seq[Byte] ) : Failable[immutable.Seq[Byte]] = {
      val ( good, pad ) = bytes.splitAt( len )
      if ( allZero( pad ) ) {
        succeed( good.toImmutableSeq )
      } else {
        fail( s"Expected byte string of length ${len}, span of nozero bytes (from left) is more than that: ${bytes.hex}" )
      }
    }
  }
  abstract class FixedLengthRepresentation[REP]( val repLen : Int ) extends Encoder[REP] {
    def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[REP,immutable.Seq[Byte]]] = {
      if ( bytes.length >= repLen ) {
        val split = bytes.splitAt( repLen )
        decodeCompleteNoLengthCheck( split._1 ).map( rep => ( rep, split._2 ) )
      } else {
        fail( s"Insufficient number of bytes, ${repLen} required, found ${bytes.length}." )
      }
    }

    def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[REP] = {
      val check = ( bytes.length == repLen ).toFailable( "A predefined byte array should be encoded as exactly ${repLen} bytes, has ${bytes.length}: ${bytes.hex}" )
      check.flatMap( _ => decodeCompleteNoLengthCheck( bytes ) )
    }

    def encodingLength : Option[Int] = Some(repLen) 

    private [Encoder] def decodeCompleteNoLengthCheck( bytes : immutable.Seq[Byte] ) : Failable[REP]
  }
}
trait Encoder[REP] {
  def parse( str : String )          : Failable[REP]
  def format( representation : REP ) : Failable[String]

  def formatAny( untypedRepresentation : Any ) : Failable[String] = {
    Failable( format( untypedRepresentation.asInstanceOf[REP] ) ).flatten // we'll see the ClassCastException in the fail object if mistyped
  }

  def encode( representation : REP )        : Failable[immutable.Seq[Byte]]
  def decode( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[REP, immutable.Seq[Byte]]]

  def encodeAny( untypedRepresentation : Any ) : Failable[immutable.Seq[Byte]] = {
    Failable( encode( untypedRepresentation.asInstanceOf[REP] ) ).flatten // we'll see the ClassCastException in the fail object if mistyped
  }

  def decodeComplete( bytes : immutable.Seq[Byte] ) : Failable[REP]

  def encodingLength : Option[Int] // will be None, signifying unknown, for dynamic types

  def parseEncode( str : String ) : Failable[immutable.Seq[Byte]] = {
    parse( str ).flatMap( encode )
  }
  def decodeFormat( bytes : immutable.Seq[Byte] ) : Failable[Tuple2[String, immutable.Seq[Byte]]] = {
    decode( bytes ).flatMap { case ( rep, rest ) =>
      format( rep ).map { strep =>
        ( strep, rest )
      }
    }
  }
}

