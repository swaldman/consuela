package com.mchange.sc.v1.consuela.trie;

import scala.annotation.tailrec;
import scala.reflect.ClassTag;

object EthStylePMTrie {
  trait UniqueSubkey[L] {
    self : Node[L,_,_] =>

    def subkey : IndexedSeq[L];
  }
  sealed trait Node[+L,+V,+H];
  case class Branch[L,V,H] ( val children : IndexedSeq[H], val mbValue : Option[V] ) extends Node[L,V,H];
  case class Extension[L,V,H] ( val subkey : IndexedSeq[L], val child : H ) extends Node[L,V,H] with UniqueSubkey[L];
  case class Leaf[L,V,H] ( val subkey : IndexedSeq[L], val value : V ) extends Node[L,V,H] with UniqueSubkey[L];
  case object Empty extends Node[Nothing,Nothing,Nothing];

  type Database[L,V,H] = PMTrie.Database[Node[L,V,H],H]
}

trait EthStylePMTrie[L,V,H] extends PMTrie[L,V,H,EthStylePMTrie.Node[L,V,H]] {

  /*
   * First lets put some unwieldy bits from the companion object into more convenient forms
   */ 
  type Node      = EthStylePMTrie.Node[L,V,H];
  type Branch    = EthStylePMTrie.Branch[L,V,H];
  type Extension = EthStylePMTrie.Extension[L,V,H];
  type Leaf      = EthStylePMTrie.Leaf[L,V,H];
  type Database  = EthStylePMTrie.Database[L,V,H];
  type Subkey    = IndexedSeq[L]

  val Empty = EthStylePMTrie.Empty;

  val Branch    = EthStylePMTrie.Branch;
  val Extension = EthStylePMTrie.Extension;
  val Leaf      = EthStylePMTrie.Leaf;

  /*
   * The following abstract members must be set by our concrete subclass
   */ 

  implicit val hashTypeClassTag : ClassTag[H];

  val alphabet : IndexedSeq[L];
  val earlyInit : ( Database, H ); /* ( Database, root at time of instance construction ) */

  /**
    *  all nodes in the updated path will already have been persisted before this method is called.
    */
  protected def instantiateSuccessor( newRootHash : H ) : Trie[L,V];

  /*
   * And now we do our work.
   */ 

  val db   : Database = earlyInit._1;
  val Zero : H      = db.Zero;
  val root : H      = earlyInit._2;
  
  lazy val rootNode = db( root );

  lazy val alphabetLen = alphabet.length;

  // useful empties
  lazy val EmptyBranchChildren = IndexedSeq.fill( alphabetLen )( Zero );
  val EmptySubkey = IndexedSeq.empty[L];

  def hash( node : Node ) : H = db.hash( node );

  def apply( key : Subkey ) : Option[V] = {
    path( key ) match {
      case exact : Path.Exact => exact.mbValue;
      case _                  => None;
    }
  }
  def including( key : Subkey, value : V ) : Trie[L,V] = {
    val updatedPath = path( key ).including( value );
    persistClone( updatedPath );
  }
  def excluding( key : Subkey ) : Trie[L,V] = {
    path( key ) match {
      case exact : Path.Exact => exact.excluding.map( persistClone( _ ) ).getOrElse( this );
      case _                  => this;
    }
  }

  def subkeys( branch : Branch ) : Seq[L] = branch.children.zip( Stream.from(0) ).filter( _._1 != Zero ).map( tup => alphabet( tup._2 ) );

  def dumpTrie : Unit = {
    def dumpNode( h : H ) : Unit = {
      val node = db( h )
      println( s"${h} -> ${node}" );
      node match {
        case Branch( children, _ ) => children.filter( _ != Zero ).foreach( dumpNode(_) );
        case Extension( _, child ) => dumpNode( child );
        case _ => /* ignore */;
      }
    }
    dumpNode( root );
  }

  private[this] def persist( updated : Path.UpdatedPath ) : Unit = {
    updated.all.foreach( element => db.put( element.hash, element.node ) );
  }
  private[this] def persistClone( updated : Path.UpdatedPath ) : Trie[L,V] = {
    persist( updated );
    updated.newRoot.fold( newTrie( Zero ) )( element => newTrie( element.hash ) )
  }

  private[this] def newTrie( newRootHash : H ) : Trie[L,V] = {
    if ( db.isInstanceOf[PMTrie.RootTracking[H]] )
      db.asInstanceOf[PMTrie.RootTracking[H]].markRoot( newRootHash );
    instantiateSuccessor( newRootHash : H ) : Trie[L,V]
  }

  private[this] def indexKidPairs( children : IndexedSeq[H] ) : IndexedSeq[( Int, H )] = Stream.from( 0 ).zip( children ).filter( _._2 != Zero ).toIndexedSeq;


  /* private[this] */ def path( key : Subkey ) : Path = Path.Builder.build( key );

  private[this] def aerr( message : String ) : Nothing = throw new AssertionError( message );

  object Path {
    object Builder {
      def build( key : IndexedSeq[L] ) : Path = if (key.isEmpty) buildEmptySubkey() else buildNonemptySubkey( root, rootNode, key, Nil )

      private def buildEmptySubkey() : Path = {
        rootNode match {
          case leaf @ Leaf( EmptySubkey, _ )               => ExactLeaf( leaf, Element.Root :: Nil );
          case leaf : Leaf                                 => EmptySubkeyAtNonemptySubkeyRootLeaf( leaf );
          case Extension( EmptySubkey, _ )                 => aerr( "Huh? Under no circumstances should we have an Extension with an empty subkey. ${rootNode}" );
          case extension @ Extension( IndexedSeq( _ ), _ ) => EmptySubkeyAtOneLetterSubkeyRootExtension( extension );
          case extension : Extension                       => EmptySubkeyAtMultiLetterSubkeyRootExtension( extension );
          case branch : Branch                             => EmptySubkeyAtRootBranch( branch );
          case Empty                                       => TruncatedAtEmptyRoot( EmptySubkey );
        }
      }
      private def buildNonemptySubkey( nodeHash : H, node : Node, searchSubkey : Subkey, parents : List[Element] ) : Path = {
        require( searchSubkey.length > 0, s"buildNonemptySubkey(...) requires a nonempty search subkey. searchSubkey -> ${searchSubkey}" );

        import SubkeyComparison._

        // build utilities
        def _fromEmpty( searchSubkey : Subkey ) : Path =  TruncatedAtEmptyRoot( searchSubkey );
        def _fromExtension( nodeHash : H, extension : Extension, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, extension.subkey );
          val nextElements = Element( nodeHash, extension ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnExtension )                 => OvershotByExtension( extension, nextElements, matched, unmatchedOnExtension );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => buildNonemptySubkey( extension.child, db( extension.child ), unmatchedOnSubkey, nextElements );
            case MatchExact( matched )                                          => ExactExtension( extension, nextElements );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnExtension )  => DivergentExtension( extension, nextElements, matched, unmatchedOnExtension, unmatchedOnSubkey );
            case NoMatch                                                        => TruncatedAtBeginningOfExtension( extension, nextElements, searchSubkey );
          }
        }
        def _fromLeaf( nodeHash : H, leaf : Leaf, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, leaf.subkey );
          val nextElements = Element( nodeHash, leaf ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnLeaf )                      => OvershotByLeaf( leaf, nextElements, matched, unmatchedOnLeaf );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => TruncatedWithinLeaf( leaf, nextElements, matched, unmatchedOnSubkey );
            case MatchExact( matched )                                          => ExactLeaf( leaf, nextElements );
            case NoMatch                                                        => TruncatedAtBeginningOfLeaf( leaf, nextElements, searchSubkey );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnLeaf )       => DivergentLeaf( leaf, nextElements, matched, unmatchedOnLeaf, unmatchedOnSubkey );
          }
        }
        def _fromBranch( nodeHash : H, branch : Branch, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val nextElements = Element( nodeHash, branch ) :: parents;
          val firstLetterIndex = alphabet.indexOf( searchSubkey.head );
          val childHash = branch.children( firstLetterIndex );
          childHash match {
            case Zero                                 => TruncatedWithinBranch( branch, nextElements, firstLetterIndex, searchSubkey.tail );
            case goodHash if searchSubkey.length == 1 => ExactBranch( branch, nextElements, firstLetterIndex );
            case goodHash                             => buildNonemptySubkey( goodHash, db( goodHash ), searchSubkey.tail, nextElements );
          }
        }

        // method implementation
        node match {
          case branch    : Branch => _fromBranch( nodeHash, branch, searchSubkey, parents );
          case extension : Extension => _fromExtension( nodeHash, extension, searchSubkey, parents );
          case leaf      : Leaf => _fromLeaf( nodeHash, leaf, searchSubkey, parents );
          case Empty => {
            assert( nodeHash == Zero, s"Huh? We were asked to build a path from the Empty node, yet it's claimed hash is nonzero? [nodeHash -> ${nodeHash}, node -> ${node}]" );
            assert( parents == Nil, s"We are asking to build a path from an Empty node which claims to have parents. [nodeHash -> ${nodeHash}, node -> ${node}, parents -> ${parents}]" );
            _fromEmpty( searchSubkey );
          }
        }
      }
      object SubkeyComparison {
        case class  MatchLessThan( matched : Subkey, unmatchedB : Subkey ) extends SubkeyComparison;
        case class  MatchGreaterThan( matched : Subkey, unmatchedA : Subkey ) extends SubkeyComparison;
        case class  MatchExact( matched : Subkey ) extends SubkeyComparison;
        case object NoMatch extends SubkeyComparison;
        case class  Divergent( val commonPrefix : Subkey, val unmatchedA : Subkey, val unmatchedB : Subkey ) extends SubkeyComparison;
      }
      sealed trait SubkeyComparison;
      def subkeyCompare( a : Subkey, b : Subkey ) : SubkeyComparison = {
        if ( a == b ) {
          SubkeyComparison.MatchExact( a )
        } else {
          val commonPrefixLen = a.zip( b ).takeWhile( tup => tup._1 == tup._2 ).length;
          if ( commonPrefixLen == 0 ) {
            SubkeyComparison.NoMatch
          } else if (a.length > commonPrefixLen ) {
            if (b.length == commonPrefixLen) {
              SubkeyComparison.MatchGreaterThan( a.take( commonPrefixLen ), a.drop( commonPrefixLen ) )
            } else {
              SubkeyComparison.Divergent( a.take( commonPrefixLen ), a.drop( commonPrefixLen ), b.drop( commonPrefixLen ) )
            }
          } else {  // a.length must equal commonPrefixLength, it can't be less than
                    // since we've ruled out MatchExact in the first comparison
            SubkeyComparison.MatchLessThan( b.take( commonPrefixLen ), b.drop( commonPrefixLen ) )
          }
        }
      }
    }
    object Element {
      lazy val Root = Element( root, rootNode );
      val Deletion = Element( Zero, null );

      def apply( node : Node ) : Element = Element( db.hash( node ), node );
    }
    case class Element( hash : H, node : Node )

    object NewElements {
      def apply( head : Element, uniqueChild : Element )   : NewElements = apply( head, Set( uniqueChild ) );
      def apply( headNode : Node )                         : NewElements = apply( Element( headNode ) );
      def apply( headNode : Node, childNodes : Set[Node] ) : NewElements = apply( Element( headNode ), childNodes.map( Element( _ ) ) );
      def apply( headNode : Node, uniqueChildNode : Node ) : NewElements = apply( headNode, Set( uniqueChildNode ) );

      val Deletion = this( Element.Deletion );
    }
    // note that the head element of a NewElements becomes the last element in modifiedPath in an UpdatedPath
    // children include any new children of that replaced last element, which also must be persisted
    case class NewElements( head : Element, children : Set[Element] = Set.empty) {
      def all : Set[Element] = children + head;
    }
    case class UpdatedPath( modifiedPath : List[Element], lastAndChildren : Option[NewElements] ) { /* Note that lastAndChildren include children of the updated path not in elements */
      assert( 
        lastAndChildren == None || lastAndChildren.get.head == modifiedPath.head, 
        s"The head of lastAndChildren should be the leaf element of our updated path. [modifiedPath -> ${modifiedPath}, lastAndChildren -> ${lastAndChildren}]" 
      );
      def all : Set[Element] = lastAndChildren.fold( modifiedPath.toSet )( _.children ++ modifiedPath ); 
      def newRoot : Option[Element] = if ( modifiedPath == Nil ) None else Some(modifiedPath.last);
    }
    private def updatePath( oldPath : List[Element], newLastElement : Element ) : List[Element] = {
      
      def _singleNodePath = newLastElement :: Nil
      def _updateMultipleElementPath : List[Element] = {

        def newParent( newChild : Element, oldChild : Element, oldParent : Element ) : Element = {
          //println( s" --> newChild -> ${ newChild }, oldChild -> ${ oldChild }, oldParent -> ${ oldParent }" );


          def updatedChildren( branch : Branch, h : H ) = branch.children.map( childHash => if ( childHash == oldChild.hash ) h else childHash );
          def culledChildren( branch : Branch )         = updatedChildren( branch, Zero );
          def replacedChildren( branch : Branch )       = updatedChildren( branch, newChild.hash );

          def deleteFromBranch( branch : Branch ) : Node = {
            val children = branch.children;
            val remainingKidPairs = indexKidPairs( children ).filter( _._2 != oldChild.hash );

            def toOneLetterExtension : Extension = { 
              val remainingKidPair = remainingKidPairs.head;
              Extension( IndexedSeq( alphabet( remainingKidPair._1 ) ), remainingKidPair._2 )
            }
            def toMaybeCondensedOneLetterExtension : Node = condenseDownward( toOneLetterExtension )/*{
              val ole = toOneLetterExtension;
              newChild.node match {
                case leaf : Leaf => Leaf( ole.subkey ++ leaf.subkey, leaf.value );
                case extension : Extension => Extension( ole.subkey ++ extension.subkey, extension.child );
                case _ => ole;
              }
            }*/
            def toOneLessChildBranch : Branch = branch.copy( children=culledChildren( branch ) )

            ( remainingKidPairs.length, branch.mbValue ) match {
              case ( 0, None )      => aerr( s"Huh? We should never see a Branch which previously had just one child and has no value! branch -> ${branch}" );
              case ( 0, someValue ) => Leaf( EmptySubkey, someValue.get ); // the Branch becomes an empty terminator
              case ( 1, None )      => toMaybeCondensedOneLetterExtension;
              case _                => toOneLessChildBranch;
            }
          }

          val DeletionNode = Element.Deletion.node; // this is just null, but to emphasize its meaning
          (newChild.node, oldParent.node) match {
            case ( DeletionNode        , _ : Extension )         => Element.Deletion
            case ( DeletionNode        , branch : Branch )       => Element( deleteFromBranch( branch ) )
            case ( leafChild : Leaf    , extension : Extension ) => Element( Leaf( extension.subkey ++ leafChild.subkey, leafChild.value ) )
            case ( extChild : Extension, ext : Extension )       => Element( Extension( ext.subkey ++ extChild.subkey, extChild.child ) )
            case ( branchChild : Branch, ext : Extension )       => Element( Extension( ext.subkey, newChild.hash ) )
            case ( _                   , branch : Branch )       => Element( branch.copy( children=replacedChildren( branch ) ) )
            case ( _                   , _ : Leaf )              => aerr( s"Leaf found as parent?! newChild->${newChild}, oldChild->${oldChild}, BAD oldParent->${oldParent}" );
            case ( _                   , Empty )                 => aerr( s"Empty node found as parent?! newChild->${newChild}, oldChild->${oldChild}, BAD oldParent->${oldParent}" );
            case ( Empty               , _ )                     => aerr( s"Empty node found as child?! newChild->${newChild}, oldChild->${oldChild}, BAD oldParent->${oldParent}" );
          }
        }

        def isLeaf( element : Element ) = {
          element.node match {
            case _ : Leaf => true;
            case _        => false;
          }
        }

        // reverseAccum builds with head closest to root and tail at or past leaf, opposite to how
        // we normally store paths.
        def accumulate( reversedAccum : List[(Element, Boolean)], oldChildParentSeq : List[Element] ) : List[(Element, Boolean)] = {
          val newP = newParent( reversedAccum.head._1, oldChildParentSeq.head, oldChildParentSeq.tail.head );
          val isL = isLeaf( newP );
          ( newP, isL ) :: reversedAccum
        }
        def reverseTruncate( accum : List[Element], reversedAccum : List[(Element, Boolean)] ) : List[Element] = {
          reversedAccum match {
            case ( Element.Deletion, _ ) :: _ => accum; // once we've hit a deletion, we have the full valid remainder of the path
            case ( elem, false ) :: Nil       => elem :: accum; // no leaf, we're done, we just return the full untruncated reversal
            case ( elem, true )  :: _         => elem :: accum;
            case ( elem, false ) :: tail      => reverseTruncate( elem :: accum, tail );
            case Nil                          => aerr( "We should never get to an empty path while reversing." );
          }
        }

        val reversedPairs = {
          oldPath
            .sliding(2, 1)
            .foldLeft( (newLastElement, isLeaf( newLastElement ) ) :: Nil )( accumulate ) // we end up with a reversed path, root in front and leaf towards tail
        }
        reverseTruncate( Nil, reversedPairs );
      }

      ( oldPath, newLastElement ) match {
        case ( Nil, Element.Deletion )      => aerr("Can't delete from an empty path!");
        case ( Nil, _ )                     => _singleNodePath;
        case ( _ :: Nil, Element.Deletion ) => Nil
        case ( _ :: Nil, _ )                => _singleNodePath;
        case _                              => _updateMultipleElementPath;
      }
    }

    def condenseDownward( parentExtension : Extension ) : Node = {
      val child = db( parentExtension.child );
      child match {
        case branch : Branch             => parentExtension;
        case leaf : Leaf                 => Leaf( parentExtension.subkey ++ leaf.subkey, leaf.value );
        case childExtension : Extension  => condenseDownward( Extension( parentExtension.subkey ++ childExtension.subkey, childExtension.child ) )
        case Empty                       => aerr( s"An Empty node should never be an Extension child! parentExtension -> ${parentExtension}" )
      }
    }
    def reshapeValueDroppingBranch( droppingBranch : Branch ) : Node = {
      val indexedKids = indexKidPairs( droppingBranch.children );
      indexedKids.length match {
        case 0 => aerr( s"We've found a Branch with no children! That should never occur in the Trie. branch->${Branch}" );
        case 1 => {
          val indexedKid = indexedKids.head;
          condenseDownward( Extension( IndexedSeq( alphabet( indexedKid._1 ) ), indexedKid._2 ) )
        }
        case _ => droppingBranch.copy( mbValue=None )
      }
    }
    def reshapeChildDroppingBranch( droppingBranch : Branch, dropLetterIndex : Int ) : Node = {
      val indexedKids = indexKidPairs( droppingBranch.children );
      ( indexedKids.length, droppingBranch.mbValue ) match {
        case (0, _)             => aerr( s"We've found a Branch with no children! That should never occur in the Trie. branch->${Branch}" );
        case (1, None)          => aerr( s"We've found a Branch with on child and no value. That should never occur in the Trie. branch->${Branch}" );
        case (1, Some( value )) => {
          val indexedKid = indexedKids.head;
          assert( dropLetterIndex == indexedKid._1, s"We're trying to drop an unfound child! dropLetterIndex -> ${dropLetterIndex}, indexedKid -> ${indexedKid}" );
          Leaf( EmptySubkey, value )
        }
        case (2, None)          => {
          val survivingChildren = indexedKids.filter( _._1 != dropLetterIndex );
          assert(
            survivingChildren.size == 1,
            s"We started with two kids and are dropping one, there should be just one. dropLetterIndex -> ${dropLetterIndex}, survivingChildren -> ${survivingChildren}"
          )
          val survivingChild = survivingChildren.head;
          condenseDownward( Extension( IndexedSeq( alphabet( survivingChild._1 ) ), survivingChild._2 ) )
        }
        case _                  => droppingBranch.copy( children=droppingBranch.children.updated( dropLetterIndex, Zero ) )
      }
    }

    case class DivergentLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, oldRemainder : Subkey, newDivergence : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val oldRemainderLeaf = Leaf( oldRemainder.tail, leaf.value ); //tail can be empty, leaves can have empty keys
        val oldRemainderLeafHash = db.hash( oldRemainderLeaf );
        val newDivergenceLeaf = Leaf( newDivergence.tail, v ); //tail can be empty, leaves can have empty keys
        val newDivergenceLeafHash = db.hash( newDivergenceLeaf );
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( oldRemainder.head ), oldRemainderLeafHash )
          .updated( alphabet.indexOf( newDivergence.head ), newDivergenceLeafHash );
        val newBranch = Branch( newBranchChildren, None );
        val newBranchHash = db.hash( newBranch );
        val currentExtension = Extension( matched, newBranchHash );
        NewElements( Element( currentExtension ), Set( Element( newBranchHash, newBranch ), Element( newDivergenceLeafHash, newDivergenceLeaf ), Element( oldRemainderLeafHash, oldRemainderLeaf ) ) )
      }
    }
    case class DivergentExtension( extension : Extension, elements : List[Element], matched : Subkey, oldRemainder : Subkey, newDivergence : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        // a bit tricky. 
        // if oldRemainder is only one long, then the new branch will consume its letter and take the extension child for its own
        // we end up with a new Leaf and a new Branch, and a leading Extension
        // otherwise, both arms of the divergence become nodes, the old one an Extension, the new one a Leaf, joined by a Branch with a leading Extension 

        // first, the easiest thing. we always need a Leaf for the new divergence.
        val newDivergenceLeaf = Leaf( newDivergence.tail, v ); //tail can be empty, leaves can have empty keys
        val newDivergenceLeafHash = db.hash( newDivergenceLeaf );

        // now, we need to condition on whether the remainder will form an Extension
        val mbOldRemainderExtensionElement : Option[Element] = if ( oldRemainder.length > 1 ) Some( Element( Extension( oldRemainder.tail, extension.child ) ) ) else None
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( oldRemainder.head ), mbOldRemainderExtensionElement.fold( extension.child )( _.hash ) )
          .updated( alphabet.indexOf( newDivergence.head ), newDivergenceLeafHash );
        val newBranch = Branch( newBranchChildren, None );
        val newBranchHash = db.hash( newBranch );
        val currentExtension = Extension( matched, newBranchHash );
        val newChildElements = Set( Element( newBranchHash, newBranch ), Element( newDivergenceLeafHash, newDivergenceLeaf ) ) ++ mbOldRemainderExtensionElement.fold( Set.empty[Element] )( Set(_) );
        NewElements( Element( currentExtension ), newChildElements )
      }
    }
    case class ExactLeaf( leaf : Leaf, elements : List[Element] ) extends Path with Exact {
      def mbValue : Option[V] = Some( leaf.value )

      // easy-peasy, no structural changes or children, just switch out the value
      def replacementForIncluding( v : V ) : NewElements = NewElements( leaf.copy( value=v ) )

      def replacementOrDeletionForExcluding : Option[NewElements] = Some( NewElements.Deletion ); // this leaf is just gone, gotta carry it up the chain
    }
    case class ExactExtension( extension : Extension, elements : List[Element] ) extends Path with Exact {
      // Per ethereum spec, Extensions must have a child branch as "terminator" if it is to be associated with a value
      def mbValue : Option[V] = {
        val child = db( extension.child );
        child match {
          case Branch( _, maybe )          => maybe;
          case Extension( EmptySubkey, _ ) => aerr( s"We expect only Branch terminators, not empty Leafs, as terminators of Extension! extension-> ${extension}, child -> ${child}" )
          case _                           => None;
        }
      }
      def replacementForIncluding( v : V ) : NewElements = {
        // an extension's child can only be a branch, otherwise it should have been merged into a much longer extension or leaf
        val childBranch : Branch = {
          db( extension.child ) match {
            case branch : Branch => branch;
            case bad             => aerr( s"An extension's child should always be a branch, instead we found: ${bad}" );
          }
        }
        val updatedChildBranch     = childBranch.copy( mbValue = Some(v) );
        val updatedChildBranchHash = db.hash( updatedChildBranch );
        val updatedExtension       = extension.copy( child=updatedChildBranchHash );
        NewElements( Element( updatedExtension ),  Element( updatedChildBranchHash, updatedChildBranch ) )
      }
      def replacementOrDeletionForExcluding : Option[NewElements] = {

        def withoutBranchValue( oldBranchChild : Branch ) = {
          val kidPairs = indexKidPairs( oldBranchChild.children );

          if ( kidPairs.length > 1 ) {
            val newBranchChild = oldBranchChild.copy( mbValue=None );
            val newBranchChildHash = db.hash( newBranchChild );
            val currentExtension = extension.copy( child=newBranchChildHash )
            NewElements( Element( currentExtension ), Element( newBranchChildHash, newBranchChild ) )
          } else {
            // condense
            val kidPair = kidPairs.head; //there had better be precisely one kidPair here
            val kidLetter = alphabet( kidPair._1 );
            val kidHash = kidPair._2;

            val onceCondensedExtension : Extension = Extension( extension.subkey :+ kidLetter, kidHash );
            val fullyCondensedNode     : Node      = condenseDownward( onceCondensedExtension )

            NewElements( fullyCondensedNode )
          }
        }

        val child = db( extension.child );
        child match {
          case oldBranchChild @ Branch( _, Some( _ ) )  => Some( withoutBranchValue( oldBranchChild ) )
          case _                                        => None;
        }
      }
    }
    case class ExactBranch( branch : Branch, elements : List[Element], matchLetterIndex : Int ) extends Path with Exact {
      // Per ethereum spec, Branches must have a child Branch or empty-key Leaf as "terminator" if it is to be associated with a value
      def mbValue : Option[V] = {
        val childHash = branch.children( matchLetterIndex );
        assert( childHash != Zero, "In an ExactBranch, the last letter should be matched, so the child should be nonzero." );
        val child = db( childHash );
        child match {
          case Branch( _, maybe )         => maybe;
          case Leaf( EmptySubkey, value ) => Some( value );
          case _                          => None;
        }
      }
      def replacementForIncluding( v : V ) : NewElements = {

        // internal utilities -- replacementForIncluding(...)
        def handleChildLeaf( childLeaf : Leaf ) : NewElements = {
          // we'll have to...
          // ...simply replace the value, if the child is an empty Leaf...
          // ...replace with a terminating Branch plus Leaf otherwise.

          // internal utilities -- replacementForIncluding >> handleChildLeaf
          def _emptyChildLeafCase = {
            val newChildLeaf = Leaf( EmptySubkey, v );
            val newChildLeafHash = db.hash( newChildLeaf );
            val newCurrentBranch = Branch( branch.children.updated( matchLetterIndex, newChildLeafHash ), branch.mbValue );
            NewElements( Element( newCurrentBranch ), Element( newChildLeafHash, newChildLeaf ) )
          }
          def _nonemptyChildLeafCase = {
            val (firstLetter, remainder) = childLeaf.subkey.splitAt(1);
            val firstLetterIndex = alphabet.indexOf( firstLetter );
            val grandchildLeaf = Leaf( remainder, childLeaf.value );
            val grandchildLeafHash = db.hash( grandchildLeaf );
            val newChildBranch = Branch( EmptyBranchChildren.updated( firstLetterIndex, grandchildLeafHash ), Some(v) );
            val newChildBranchHash = db.hash( newChildBranch );
            val newCurrentBranch = Branch( branch.children.updated( matchLetterIndex, newChildBranchHash ), branch.mbValue );
            NewElements( Element( newCurrentBranch ), Set( Element( newChildBranchHash, newChildBranch ), Element( grandchildLeafHash, grandchildLeaf ) ) )
          }

          // method implementation -- replacementForIncluding >> handleChildLeaf
          childLeaf.subkey.length match {
            case 0 => _emptyChildLeafCase;
            case _ => _nonemptyChildLeafCase;
          }
        }
        def handleChildExtension( childExtension : Extension ) : NewElements = {
          // we'll have to... 
          // ...turn the child Extension into a terminating Branch if it consumes only one nibble
          // ...turn the child Extension into a terminating Branch plus Extension if it consumes more than one nibble

          //internal utilities -- replacementForIncluding >> handleChildExtension
          def _oneLetterCase = { //we turn childExtension into a Branch, which terminates the current Branch with a value
            val letter = childExtension.subkey.head;
            val letterIndex = alphabet.indexOf( letter );
            val newChildBranchChildren = EmptyBranchChildren.updated( letterIndex, childExtension.child );
            val newChildBranch = Branch( newChildBranchChildren, Some( v ) );
            val newChildBranchHash = db.hash( newChildBranch );
            val newCurrentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildBranchHash ) );
            NewElements( Element( newCurrentBranch ), Element( newChildBranchHash, newChildBranch ) )
          }
          def _multiLetterCase = {
            // we reorganize childExtension into a Branch, which terminates the current Branch with a value, and a new Extension child of the new child Branch
            val firstLetter = childExtension.subkey.head;
            val firstLetterIndex = alphabet.indexOf( firstLetter );
            val newGrandchildExtensionSubkey = childExtension.subkey.tail;
            val newGrandchildExtensionChild = childExtension.child;
            val newGrandchildExtension = Extension( newGrandchildExtensionSubkey, newGrandchildExtensionChild )
            val newGrandchildExtensionHash = db.hash( newGrandchildExtension );
            val newChildBranchChildren = EmptyBranchChildren.updated( firstLetterIndex, newGrandchildExtensionHash );
            val newChildBranch = Branch( newChildBranchChildren, Some( v ) );
            val newChildBranchHash = db.hash( newChildBranch );
            val newCurrentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildBranchHash ) );
            NewElements( Element( newCurrentBranch ), Set( Element( newChildBranchHash, newChildBranch ), Element( newGrandchildExtensionHash, newGrandchildExtension ) ) )
          }

          // method implementation -- replacementForIncluding >> handleChildExtension
          val ceskLen = childExtension.subkey.length;
          if ( ceskLen < 1 ) aerr( s"Bad childExtension! Empty subkey Extensions are prohibited by the Ethereum spec. childExtension -> ${childExtension}" );

          ceskLen match {
            case 1 => _oneLetterCase;
            case _ => _multiLetterCase;
          }
        }
        def handleChildBranch( childBranch : Branch ) : NewElements = {
          val updatedChildBranch     = childBranch.copy( mbValue = Some(v) );
          val updatedChildBranchHash = db.hash( updatedChildBranch );
          val updatedMainBranch      = branch.copy( children=branch.children.updated( matchLetterIndex, updatedChildBranchHash ) );
          NewElements( Element( updatedMainBranch ), Element( updatedChildBranchHash, updatedChildBranch ) )
        }

        // method implementation -- replacementForIncluding
        val childHash = branch.children( matchLetterIndex );
        assert( childHash != Zero, s"To be an ExactBranch match, the branch should have consumed the match letter, so its hash should not be Zero: childHash -> ${childHash}" );
        val childNode = db( childHash );
        childNode match {
          case childLeaf      : Leaf      => handleChildLeaf( childLeaf );
          case childExtension : Extension => handleChildExtension( childExtension );
          case childBranch    : Branch    => handleChildBranch( childBranch );
          case Empty                      => aerr( s"A lookup of a nonzero hash should not return Empty! childHash -> ${childHash}" );
        }
      }

      def replacementOrDeletionForExcluding : Option[NewElements] = {

        def removeChild = Some( NewElements( reshapeChildDroppingBranch( branch, matchLetterIndex ) ) )

        def reshapeChildBranch( childBranch : Branch )  = {
          val newChild = reshapeValueDroppingBranch( childBranch );
          val newChildHash = db.hash( newChild );
          val currentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildHash ) )
          Some( NewElements( Element( currentBranch ), Element( newChildHash, newChild ) ) )
        }
        val potentiallyTerminatingChild = db( branch.children( matchLetterIndex ) );
        potentiallyTerminatingChild match {
          case Leaf( EmptySubkey, _ )               => removeChild;
          case childBranch @ Branch( _, Some( _ ) ) => reshapeChildBranch( childBranch )
          case _                                    => None; //not a terminator
        }
      }
    }
    case class OvershotByLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        // we have to replace one character of the Leaf by a Branch, which will terminate the matched subkey.
        // matched elements form a leading Extension

        val remainderLeaf = Leaf( remainder.tail, leaf.value ); // subkey can be empty, expected for leaves
        val remainderLeafHash = db.hash( remainderLeaf );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( remainder.head ), remainderLeafHash ), Some( v ) );
        val newBranchHash = db.hash( newBranch );
        val currentExtension = Extension( matched, newBranchHash );
        NewElements( Element( currentExtension ), Set( Element( newBranchHash, newBranch ), Element( remainderLeafHash, remainderLeaf ) ) )
      }
    }
    case class OvershotByExtension( extension : Extension, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        // we make a Branch to terminate the matched portion
        // if the remainder is larger than one, we have to replace it with an Extension, otherwise the Branch suffices

        val mbRemainderExtensionElement = if (remainder.length > 1) Some( Element( Extension( remainder.tail, extension.child ) ) ) else None;
        val branchChildHash = mbRemainderExtensionElement.fold( extension.child )( _.hash );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( remainder.head ), branchChildHash ), Some(v) );
        val newBranchHash = db.hash( newBranch );
        val newBranchElement = Element( newBranchHash, newBranch );
        val currentExtension = Extension( matched, newBranchHash );
        NewElements( Element(currentExtension), mbRemainderExtensionElement.fold( Set( newBranchElement ) )( remainderElement => Set( newBranchElement, remainderElement ) ) )
      }
    }
    case class TruncatedAtEmptyRoot( remainder : Subkey ) extends Path {
      val elements : List[Element] = Nil;

      def replacementForIncluding( v : V ) : NewElements = NewElements( Leaf( remainder, v ) ); //easy-peasy...
    }
    case class TruncatedAtBeginningOfLeaf( leaf : Leaf, elements : List[Element], newLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {

        def emptyLeafSubkeyCase = {
          val newChildLeaf = Leaf( newLetters.tail, v );
          val newChildLeafHash = db.hash( newChildLeaf );
          val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( newLetters.head ), newChildLeafHash ), Some( leaf.value ) );
          NewElements( Element( newBranch ), Element( newChildLeaf ) )
        }

        def nonEmptyLeafSubkeyCase = {
          val newLettersLeaf = Leaf( newLetters.tail, v );
          val newLettersLeafHash = db.hash( newLettersLeaf );
          val oldLetters = leaf.subkey;
          val oldLettersLeaf = Leaf( oldLetters.tail, leaf.value );
          val oldLettersLeafHash = db.hash( oldLettersLeaf );
          val newBranchChildren = EmptyBranchChildren
            .updated( alphabet.indexOf( newLetters.head ), newLettersLeafHash )
            .updated( alphabet.indexOf( oldLetters.head ), oldLettersLeafHash );
          val newBranch = Branch( newBranchChildren, None );
          NewElements( Element( newBranch ), Set( Element( oldLettersLeafHash, oldLettersLeaf ), Element( newLettersLeafHash, newLettersLeaf ) ) )
        }

        if ( leaf.subkey.isEmpty ) emptyLeafSubkeyCase else nonEmptyLeafSubkeyCase
      }
    }
    case class TruncatedAtBeginningOfExtension( extension : Extension, elements : List[Element], newLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val newLettersLeaf = Leaf( newLetters.tail, v );
        val newLettersLeafHash = db.hash( newLettersLeaf );
        val newLettersLeafElement = Element( newLettersLeafHash, newLettersLeaf );
        val oldLetters = extension.subkey;
        val mbOldLettersExtensionElement = if ( oldLetters.length > 1 ) Some( Element( Extension( oldLetters.tail, extension.child ) ) ) else None;
        val oldLettersBranchChild = mbOldLettersExtensionElement.fold( extension.child )( _.hash );
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( newLetters.head ), newLettersLeafHash )
          .updated( alphabet.indexOf( oldLetters.head ), oldLettersBranchChild );
        val newBranch = Branch( newBranchChildren, None );
        NewElements( Element( newBranch ), mbOldLettersExtensionElement.fold( Set( newLettersLeafElement ) )( oldLettersExtensionElement => Set( newLettersLeafElement, oldLettersExtensionElement ) ) )
      }
    }
    case class TruncatedWithinLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, newLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        // the existing Leaf has to become an Extension terminated by a Branch
        // to which a new Leaf with the new Letters must be added

        val newLettersLeaf = Leaf( newLetters.tail, v );
        val newLettersLeafHash = db.hash( newLettersLeaf );
        val joinBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( newLetters.head ), newLettersLeafHash ), Some( leaf.value ) );
        val joinBranchHash = db.hash( joinBranch );
        val currentExtension = Extension( matched, joinBranchHash );
        NewElements( Element( currentExtension ), Set( Element( joinBranchHash, joinBranch ), Element( newLettersLeafHash, newLettersLeaf ) ) )
      }
    }
    case class TruncatedWithinBranch( branch : Branch, elements : List[Element], branchLetterIndex : Int, extraLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val extraLettersLeaf = Leaf( extraLetters, v );
        val extraLettersLeafHash = db.hash( extraLettersLeaf );
        def msg = s"Huh? Truncation within a branch means that trying to traverse our next letter hits an empty child! branchLetterIndex -> ${branchLetterIndex}, branch -> ${branch}";
        assert( branch.children( branchLetterIndex ) == Zero, msg );
        val newBranchChildren = branch.children.updated( branchLetterIndex, extraLettersLeafHash );
        val newBranch = Branch( newBranchChildren, branch.mbValue );
        NewElements( Element( newBranch ), Element( extraLettersLeafHash, extraLettersLeaf ) )
      }
    }
    case class EmptySubkeyAtNonemptySubkeyRootLeaf( rootLeaf : Leaf ) extends Path {
      def elements = Element( rootLeaf ) :: Nil;

      def replacementForIncluding( v : V ) : NewElements = {
        val childLeaf = Leaf( rootLeaf.subkey.tail, rootLeaf.value );
        val childLeafHash = db.hash( childLeaf );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( rootLeaf.subkey.head ), childLeafHash ), Some( v ) );
        NewElements( Element( newBranch ), Element( childLeafHash, childLeaf ) )
      }
    }
    case class EmptySubkeyAtOneLetterSubkeyRootExtension( rootExtension : Extension ) extends Path {
      def elements = Element( rootExtension ) :: Nil;

      def replacementForIncluding( v : V ) : NewElements = {
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( rootExtension.subkey.head ), rootExtension.child ), Some( v ) );
        NewElements( Element( newBranch ) )
      }
    }
    case class EmptySubkeyAtMultiLetterSubkeyRootExtension( rootExtension : Extension ) extends Path {
      def elements = Element( rootExtension ) :: Nil;

      def replacementForIncluding( v : V ) : NewElements = {
        val childExtension = Extension( rootExtension.subkey.tail, rootExtension.child );
        val childExtensionHash = db.hash( childExtension );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( rootExtension.subkey.head ), childExtensionHash ), Some( v ) );
        NewElements( Element( newBranch ), Element( childExtensionHash, childExtension ) )
      }
    }
    case class EmptySubkeyAtRootBranch( rootBranch : Branch ) extends Path with Exact {
      def elements = Element( rootBranch ) :: Nil;

      def mbValue   : Option[V] = rootBranch.mbValue;

      def replacementForIncluding( v : V ) : NewElements = NewElements( rootBranch.copy( mbValue=Some(v) ) )

      def replacementOrDeletionForExcluding : Option[NewElements] = {
        def oneChildCase( childIndex : Int ) = {
          val subkeyLetter = alphabet( childIndex );
          val childHash = rootBranch.children( childIndex );
          val childNode = db( childHash );

          def branchChildCase = NewElements( Extension( IndexedSeq( subkeyLetter ), childHash ) );
          def leafChildCase( leafChild : Leaf ) = NewElements( Leaf( subkeyLetter +: leafChild.subkey, leafChild.value ) )
          def extensionChildCase( extensionChild : Extension ) = NewElements( Extension( subkeyLetter +: extensionChild.subkey, extensionChild.child ) )

          childNode match {
            case _ : Branch                 => branchChildCase;
            case leafChild : Leaf           => leafChildCase( leafChild );
            case extensionChild : Extension => extensionChildCase( extensionChild );
            case Empty                      => aerr( s"A one-child branch shouldn't have an empty node as its child! rootBranch -> ${rootBranch}" );
          }
        }
        def multipleChildCase = NewElements( rootBranch.copy( mbValue=None ) )

        val kidPairs = indexKidPairs( rootBranch.children );
        kidPairs.length match {
          case 0 => aerr( s"We should never see a zero-child Branch! rootBranch->${rootBranch}" );
          case 1 => Some( oneChildCase( kidPairs.head._1 ) );
          case _ => Some( multipleChildCase );
        }
      }
    }

    trait Exact {
      self : Path =>

      def mbValue   : Option[V]

      def excluding : Option[UpdatedPath] = replacementOrDeletionForExcluding.map( updatedPath(_) )
      def replacementOrDeletionForExcluding : Option[NewElements];
    }
  }
  sealed trait Path {
    import Path._;
    def elements  : List[Element]; // head is closest to leaf, last is root
    def replacementForIncluding( value : V ) : NewElements;

    def including( value : V ) : UpdatedPath = updatedPath( replacementForIncluding( value ) );

    def updatedPath( newElements : NewElements ) : UpdatedPath = {
      val newPath = Path.updatePath( elements, newElements.head );
      val lastAndChildren = {
        if ( newElements == NewElements.Deletion ) {
          None
        } else if ( newPath.isEmpty ) {
          None
        } else if (newPath.head == newElements.head ) {
          Some( newElements )
        } else {
          // updating the path condensed the last node, we have to do some work to find the children of the condensed node
          newPath.head match {
            case Element(_, branch : Branch)       => aerr( s"A Branch could not have been the result of a condensation! branch -> ${branch}" );
            case Element(_, leaf : Leaf)           => None; // no kids to worry about
            case Element(_, extension : Extension) => {
              def referenced = {
                def referencedFrom( referencer : Node ) : Set[Element] = referencer match {
                  case _         : Leaf      => Set.empty[Element];
                  case extension : Extension => newElements.children.filter( extension.child == _.hash );
                  case branch    : Branch    => newElements.children.filter( elem => branch.children.contains( elem.hash ) );
                  case Empty                 => aerr( "We should never find an empty node in a path or as its children!" );
                }
                @tailrec
                def accumulateReferenced( accum : Set[Element], referencers : Set[Element] ) : Set[Element] = {
                  val newReferenced = referencers.foldLeft( Set.empty[Element] )( ( set, referencer ) => set ++ referencedFrom( referencer.node ) );
                  if ( newReferenced.isEmpty ) accum else accumulateReferenced( accum ++ newReferenced, newReferenced )
                }
                accumulateReferenced( Set.empty, Set(newPath.head) );
              }

              val unhandledChildren = referenced;
              if ( unhandledChildren.isEmpty ) None else Some( NewElements( newPath.head, unhandledChildren ) )
            }
          }
        }
      }
      UpdatedPath( newPath, lastAndChildren )
    }
  }
}
