package com.mchange.sc.v1.consuela.trie;

import scala.annotation.tailrec;

object EmbeddableEthStylePMTrie {

  sealed trait Node[+L,+V,+H];
  case class Branch[L,V,H] ( val children : IndexedSeq[NodeSource[L,V,H]], val mbValue : Option[V] ) extends Node[L,V,H];
  case class Extension[L,V,H] ( val subkey : IndexedSeq[L], val child : NodeSource[L,V,H] ) extends Node[L,V,H] with UniqueSubkey[L,V,H];
  case class Leaf[L,V,H] ( val subkey : IndexedSeq[L], val value : V ) extends Node[L,V,H] with UniqueSubkey[L,V,H];
  case object Empty extends Node[Nothing,Nothing,Nothing];

  trait UniqueSubkey[L,V,H] {
    self : Node[L,V,H] =>

    def subkey : IndexedSeq[L];
  }
  object NodeSource {
    trait Defaults {
      def isHash : Boolean     = false;
      def isEmbedded : Boolean = false;
      def isEmpty : Boolean    = false;
    }
    case class Hash[L,V,H]( hash : H ) extends Defaults with NodeSource[L,V,H]               { override def isHash     : Boolean = true; }
    case class Embedded[L,V,H]( node : Node[L,V,H] ) extends Defaults with NodeSource[L,V,H] { override def isEmbedded : Boolean = true; }
    case object Empty extends Defaults with NodeSource[Nothing,Nothing,Nothing]                    { override def isEmpty    : Boolean = true; }
  }
  sealed trait NodeSource[+L,+V,+H]{
    def isHash : Boolean;
    def isEmbedded : Boolean;
    def isEmpty : Boolean;
  }

  trait Database[L,V,H] {
    type Node       = EmbeddableEthStylePMTrie.Node[L,V,H];
    type NodeSource = EmbeddableEthStylePMTrie.NodeSource[L,V,H];
    type Branch     = EmbeddableEthStylePMTrie.Branch[L,V,H];
    type Extension  = EmbeddableEthStylePMTrie.Extension[L,V,H];
    type Leaf       = EmbeddableEthStylePMTrie.Leaf[L,V,H];
    type Subkey     = IndexedSeq[L]

    val NodeSource = EmbeddableEthStylePMTrie.NodeSource;
    val Branch     = EmbeddableEthStylePMTrie.Branch;
    val Extension  = EmbeddableEthStylePMTrie.Extension;
    val Leaf       = EmbeddableEthStylePMTrie.Leaf;
    val Empty      = EmbeddableEthStylePMTrie.Empty;

    // definitely requires access to the persistent store
    def put( hash : H, node : Node ) : Unit;
    def apply( hash : H ) : Node;

    // may require access to the persistent store
    def dereference( node : NodeSource ) : Node;

    // no access to the persistent store
    def reference( node : Node )     : NodeSource;
    def rootReference( node : Node ) : NodeSource; // same, a reference, but cannot be embedded, since there's nothing to embed root
    def EmptyHash : H;
  }

  case class EarlyInit[L,V,H]( alphabet : IndexedSeq[L], database : Database[L,V,H], RootHash : H );
}

trait EmbeddableEthStylePMTrie[L,V,H] extends PMTrie[L,V,H] {


  type Node       = EmbeddableEthStylePMTrie.Node[L,V,H];
  type Branch     = EmbeddableEthStylePMTrie.Branch[L,V,H];
  type Extension  = EmbeddableEthStylePMTrie.Extension[L,V,H];
  type Leaf       = EmbeddableEthStylePMTrie.Leaf[L,V,H];
  type NodeSource = EmbeddableEthStylePMTrie.NodeSource[L,V,H];
  type Database   = EmbeddableEthStylePMTrie.Database[L,V,H];
  type Subkey     = IndexedSeq[L]

  val Branch     = EmbeddableEthStylePMTrie.Branch;
  val Extension  = EmbeddableEthStylePMTrie.Extension;
  val Leaf       = EmbeddableEthStylePMTrie.Leaf;

  import EmbeddableEthStylePMTrie.NodeSource;
  import EmbeddableEthStylePMTrie.Empty;


  /*
   * The following abstract members must be set by our concrete subclass
   */ 

  /**
   *  define in an early initializer!
   */ 
  val earlyInit : EmbeddableEthStylePMTrie.EarlyInit[L,V,H]

  /**
    *  all nodes in the updated path will already have been persisted before this method is called.
    */
  protected def instantiateSuccessor( newRootHash : H ) : Trie[L,V];

  /*
   * And now we do our work.
   */ 

  val alphabet   : IndexedSeq[L] = earlyInit.alphabet;
  val db         : Database      = earlyInit.database;
  val RootHash       : H             = earlyInit.RootHash;
  
  val alphabetLen = alphabet.length;

  lazy val RootSource = if ( RootHash == db.EmptyHash ) NodeSource.Empty else NodeSource.Hash( RootHash );
  lazy val RootNode   = db.dereference( RootSource );

  // useful empties
  val EmptyBranchChildren = IndexedSeq.fill( alphabetLen )( NodeSource.Empty );
  val EmptySubkey = IndexedSeq.empty[L];

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

  def EmptyHash = db.EmptyHash;

  def subkeys( branch : Branch ) : Seq[L] = branch.children.zip( Stream.from(0) ).filter( _._1 != NodeSource.Empty ).map( tup => alphabet( tup._2 ) );

  def dumpTrie : Unit = {
    def dumpNode( nodeSource : NodeSource ) : Unit = {
      val node = db.dereference( nodeSource )
      println( s"${nodeSource} -> ${node}" );
      node match {
        case Branch( children, _ ) => children.filter( _.isHash ).foreach( dumpNode(_) ); // embedded and empties have already printed
        case Extension( _, child ) if ( child.isHash ) => dumpNode( child );              // embedded and empties have already printed
        case _ => /* ignore */;
      }
    }
    dumpNode( RootSource );
  }

/*
  private[this] def persist( element : Element, isRoot : Boolean = false ) : Unit = {
    ( element, isRoot ) match {
      case ( Element( NodeSource.Hash( hash ), node ), _ ) => db.put( hash, node );
      case ( Element( NodeSource.Embedded( _ ), node ), true ) => db.put( rootReference( node ).hash, node );
      case ( _, false ) => aerr( s"Only (nonroot) nodes that reference via Hashes can be directly persisted. node->${node}, source->${source}" );
    }
  }
 */

  // ugly, very verbose fully qualified names to avoid warnings like 
  // "The outer reference in this type test cannot be checked at run time."
  def persist( updated : Path.UpdatedPath ) : Option[H] = {
    def rootReferenceHash( rootRef : NodeSource ) : H = {
      rootRef match {
        case NodeSource.Hash( hash ) => hash;
        case NodeSource.Embedded( _ ) => aerr( s"A root reference cannot be an embedded node. It must be referenced by a hash. rootRef -> ${rootRef}" );
        case NodeSource.Empty => db.EmptyHash;
      }
    }
    val NewRootElement : Path.Element = updated.newRoot.getOrElse( null ); //so sue me
    val mbRootHash = NewRootElement match {
      case Path.Element( NodeSource.Hash( hash ), _ )     => Some( hash );
      case Path.Element( NodeSource.Embedded( _ ), node ) => Some( rootReferenceHash( db.rootReference( node ) ) );
      case null => None
      case _    => aerr( s"updated.newRoot is an Element that should never appear in a path. NewRootElement -> ${NewRootElement}" );
    }

    // take advantage of BulkWriting databases
    // note this is an unleaked local method, a
    // single Thread context. we'll use mutable
    // Maps with abandon.
    trait Puttable {
      def put( hash : H, node : Node ) : Unit;
      def flush() : Unit;
    }
    val puttable = {
      db match {
        case bwdb : PMTrie.Database.BulkWriting[Node @unchecked , H @unchecked] => { // this is safe, the self-type will have enforced consistency
          new Puttable {
            import scala.collection._;
            val mutableMap = mutable.Map.empty[H,Node];
            def put( hash : H, node : Node ) : Unit = mutableMap += ( hash -> node );
            def flush() : Unit = bwdb.put( mutableMap.toMap ) 
          }
        }
        case _ => { 
          new Puttable {
            def put( hash : H, node : Node ) : Unit = db.put( hash, node );
            def flush() : Unit = (); // no op
          }
        }
      }
    }
    updated.all.foreach { element => 
      element match {
        case NewRootElement                                => puttable.put( mbRootHash.get, NewRootElement.node );
        case Path.Element( NodeSource.Hash( hash ), node ) => puttable.put( hash, node );
        case Path.Element( NodeSource.Embedded( _ ), _ )   => /* skip. this will be persisted via a parent */;
        case _                                             => aerr( s"Unexpected element in persist(...). element -> ${element}" );
      }
    }
    puttable.flush();
    mbRootHash
  }
  private[this] def persistClone( updated : Path.UpdatedPath ) : Trie[L,V] = {
    val mbRootHash = persist( updated );
    mbRootHash.fold( newTrie( db.EmptyHash ) )( newTrie( _ ) )
  }

  private[this] def newTrie( newRootHash : H ) : Trie[L,V] = {
//    if ( db.isInstanceOf[PMTrie.RootTracking[H]] )
//      db.asInstanceOf[PMTrie.RootTracking[H]].markRoot( newRootHash );
    instantiateSuccessor( newRootHash : H ) : Trie[L,V]
  }

  private[this] def indexKidPairs( children : IndexedSeq[NodeSource] ) : IndexedSeq[( Int, NodeSource )] = Stream.from( 0 ).zip( children ).filter( _._2 != NodeSource.Empty ).toIndexedSeq;


  /* private[this] */ def path( key : Subkey ) : Path = Path.Builder.build( key );

  private[this] def aerr( message : String ) : Nothing = throw new AssertionError( message );

  object Path {
    object Builder {
      def build( key : Subkey ) : Path = if (key.isEmpty) buildEmptySubkey() else buildNonemptySubkey( RootSource, RootNode, key, Nil )

      private def buildEmptySubkey() : Path = {
        RootNode match {
          case leaf @ Leaf( EmptySubkey, _ )               => ExactLeaf( leaf, Element.Root :: Nil );
          case leaf : Leaf                                 => EmptySubkeyAtNonemptySubkeyRootLeaf( leaf );
          case Extension( EmptySubkey, _ )                 => aerr( "Huh? Under no circumstances should we have an Extension with an empty subkey. ${RootNode}" );
          case extension @ Extension( IndexedSeq( _ ), _ ) => EmptySubkeyAtOneLetterSubkeyRootExtension( extension );
          case extension : Extension                       => EmptySubkeyAtMultiLetterSubkeyRootExtension( extension );
          case branch : Branch                             => EmptySubkeyAtRootBranch( branch );
          case Empty                                       => TruncatedAtEmptyRoot( EmptySubkey );
        }
      }
      private def buildNonemptySubkey( nodeSource : NodeSource, node : Node, searchSubkey : Subkey, parents : List[Element] ) : Path = {
        require( searchSubkey.length > 0, s"buildNonemptySubkey(...) requires a nonempty search subkey. searchSubkey -> ${searchSubkey}" );

        import SubkeyComparison._

        // build utilities
        def _fromEmpty( searchSubkey : Subkey ) : Path =  TruncatedAtEmptyRoot( searchSubkey );
        def _fromExtension( nodeSource : NodeSource, extension : Extension, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, extension.subkey );
          val nextElements = Element( nodeSource, extension ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnExtension )                 => OvershotByExtension( extension, nextElements, matched, unmatchedOnExtension );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => buildNonemptySubkey( extension.child, db.dereference( extension.child ), unmatchedOnSubkey, nextElements );
            case MatchExact( matched )                                          => ExactExtension( extension, nextElements );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnExtension )  => DivergentExtension( extension, nextElements, matched, unmatchedOnExtension, unmatchedOnSubkey );
            case NoMatch                                                        => TruncatedAtBeginningOfExtension( extension, nextElements, searchSubkey );
          }
        }
        def _fromLeaf( nodeSource : NodeSource, leaf : Leaf, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, leaf.subkey );
          val nextElements = Element( nodeSource, leaf ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnLeaf )                      => OvershotByLeaf( leaf, nextElements, matched, unmatchedOnLeaf );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => TruncatedWithinLeaf( leaf, nextElements, matched, unmatchedOnSubkey );
            case MatchExact( matched )                                          => ExactLeaf( leaf, nextElements );
            case NoMatch                                                        => TruncatedAtBeginningOfLeaf( leaf, nextElements, searchSubkey );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnLeaf )       => DivergentLeaf( leaf, nextElements, matched, unmatchedOnLeaf, unmatchedOnSubkey );
          }
        }
        def _fromBranch( nodeSource : NodeSource, branch : Branch, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val nextElements = Element( nodeSource, branch ) :: parents;
          val firstLetterIndex = alphabet.indexOf( searchSubkey.head );
          val childSource = branch.children( firstLetterIndex );
          childSource match {
            case NodeSource.Empty                       => TruncatedWithinBranch( branch, nextElements, firstLetterIndex, searchSubkey.tail );
            case goodSource if searchSubkey.length == 1 => ExactBranch( branch, nextElements, firstLetterIndex );
            case goodSource                             => buildNonemptySubkey( goodSource, db.dereference( goodSource ), searchSubkey.tail, nextElements );
          }
        }

        // method implementation
        node match {
          case branch    : Branch => _fromBranch( nodeSource, branch, searchSubkey, parents );
          case extension : Extension => _fromExtension( nodeSource, extension, searchSubkey, parents );
          case leaf      : Leaf => _fromLeaf( nodeSource, leaf, searchSubkey, parents );
          case Empty => {
            assert( nodeSource == NodeSource.Empty, s"Huh? We were asked to build a path from the Empty node, yet it's claimed hash is nonzero? [nodeSource -> ${nodeSource}, node -> ${node}]" );
            assert( parents == Nil, s"We are asking to build a path from an Empty node which claims to have parents. [nodeSource -> ${nodeSource}, node -> ${node}, parents -> ${parents}]" );
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
      val Root = Element( RootSource, RootNode );
      val Deletion = Element( NodeSource.Empty, null );

      def apply( node : Node ) : Element = Element( db.reference( node ), node );
    }
    case class Element( source : NodeSource, node : Node )

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
    /* Note that lastAndChildren include children of the updated path not in modifiedPath */
    /* newRoot is in modifiedPath, but is captured separately to avoid an extra list traversal */
    case class UpdatedPath( modifiedPath : List[Element], lastAndChildren : Option[NewElements], newRoot : Option[Element] ) { 
      assert( 
        lastAndChildren == None || lastAndChildren.get.head == modifiedPath.head, 
        s"The head of lastAndChildren should be the leaf element of our updated path. [modifiedPath -> ${modifiedPath}, lastAndChildren -> ${lastAndChildren}]" 
      );

      // note that newRoot is already in modifiedPath. we've captured it separately to avoid anothe list traversal
      // but we don't want to double-include it in all
      def all : Set[Element] = lastAndChildren.fold( modifiedPath.toSet )( _.children ++ modifiedPath ); 

      //lazy val newRoot : Option[Element] = if ( modifiedPath == Nil ) None else Some(modifiedPath.last);
    }
    private def updatePath( oldPath : List[Element], newLastElement : Element ) : ( Option[Element], List[Element] ) = { // ( new root node, new path )
      
      def _singleNodePath = ( Some(newLastElement), newLastElement :: Nil )
      def _updateMultipleElementPath : (Option[Element], List[Element]) = {

        def newParent( newChild : Element, oldChild : Element, oldParent : Element ) : Element = {
          //println( s" --> newChild -> ${ newChild }, oldChild -> ${ oldChild }, oldParent -> ${ oldParent }" );

          def updatedChildren( branch : Branch, newSource : NodeSource ) = branch.children.map( childSource => if ( childSource == oldChild.source ) newSource else childSource );
          def culledChildren( branch : Branch )                          = updatedChildren( branch, NodeSource.Empty );
          def replacedChildren( branch : Branch )                        = updatedChildren( branch, newChild.source );

          // remember that "newChild" is Deletion in this case, don't try to condense with it!
          def deleteFromBranch( branch : Branch ) : Node = {
            val children = branch.children;
            val remainingKidPairs = indexKidPairs( children ).filter( _._2 != oldChild.source );

            def toOneLetterExtension : Extension = { 
              val remainingKidPair = remainingKidPairs.head;
              Extension( IndexedSeq( alphabet( remainingKidPair._1 ) ), remainingKidPair._2 )
            }
            def toMaybeCondensedOneLetterExtension : Node = condenseDownward( toOneLetterExtension )

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
            case ( branchChild : Branch, ext : Extension )       => Element( Extension( ext.subkey, newChild.source ) )
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
        def reverseTruncate( knownRoot : Option[Element], accum : List[Element], reversedAccum : List[(Element, Boolean)] ) : (Option[Element], List[Element]) = {
          val foundRoot = knownRoot.orElse( Some(reversedAccum.head._1) ); // the first time we run through this, the element at head of reversedAccum is root!
          reversedAccum match {
            case ( Element.Deletion, _ ) :: _ => ( foundRoot, accum ); // once we've hit a deletion, we have the full valid remainder of the path
            case ( elem, false ) :: Nil       => ( foundRoot, elem :: accum ); // no leaf, we're done, we just return the full untruncated reversal
            case ( elem, true )  :: _         => ( foundRoot, elem :: accum );
            case ( elem, false ) :: tail      => reverseTruncate( foundRoot, elem :: accum, tail );
            case Nil                          => aerr( "We should never get to an empty path while reversing." );
          }
        }

        val reversedPairs = {
          oldPath
            .sliding(2, 1)
            .foldLeft( (newLastElement, isLeaf( newLastElement ) ) :: Nil )( accumulate ) // we end up with a reversed path, root in front and leaf towards tail
        }
        reverseTruncate( None, Nil, reversedPairs );
      }

      ( oldPath, newLastElement ) match {
        case ( Nil, Element.Deletion )      => aerr("Can't delete from an empty path!");
        case ( Nil, _ )                     => _singleNodePath;
        case ( _ :: Nil, Element.Deletion ) => (None, Nil)
        case ( _ :: Nil, _ )                => _singleNodePath;
        case _                              => _updateMultipleElementPath;
      }
    }

    def condenseDownward( parentExtension : Extension ) : Node = {
      val child = db.dereference( parentExtension.child );
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
        case _                  => droppingBranch.copy( children=droppingBranch.children.updated( dropLetterIndex, NodeSource.Empty ) )
      }
    }

    case class DivergentLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, oldRemainder : Subkey, newDivergence : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val oldRemainderLeaf = Leaf( oldRemainder.tail, leaf.value ); //tail can be empty, leaves can have empty keys
        val oldRemainderLeafSource = db.reference( oldRemainderLeaf );
        val newDivergenceLeaf = Leaf( newDivergence.tail, v ); //tail can be empty, leaves can have empty keys
        val newDivergenceLeafSource = db.reference( newDivergenceLeaf );
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( oldRemainder.head ), oldRemainderLeafSource )
          .updated( alphabet.indexOf( newDivergence.head ), newDivergenceLeafSource );
        val newBranch = Branch( newBranchChildren, None );
        val newBranchSource = db.reference( newBranch );
        val currentExtension = Extension( matched, newBranchSource );
        NewElements( Element( currentExtension ), Set( Element( newBranchSource, newBranch ), Element( newDivergenceLeafSource, newDivergenceLeaf ), Element( oldRemainderLeafSource, oldRemainderLeaf ) ) )
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
        val newDivergenceLeafSource = db.reference( newDivergenceLeaf );

        // now, we need to condition on whether the remainder will form an Extension
        val mbOldRemainderExtensionElement : Option[Element] = if ( oldRemainder.length > 1 ) Some( Element( Extension( oldRemainder.tail, extension.child ) ) ) else None
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( oldRemainder.head ), mbOldRemainderExtensionElement.fold( extension.child )( _.source ) )
          .updated( alphabet.indexOf( newDivergence.head ), newDivergenceLeafSource );
        val newBranch = Branch( newBranchChildren, None );
        val newBranchSource = db.reference( newBranch );
        val currentExtension = Extension( matched, newBranchSource );
        val newChildElements = Set( Element( newBranchSource, newBranch ), Element( newDivergenceLeafSource, newDivergenceLeaf ) ) ++ mbOldRemainderExtensionElement.fold( Set.empty[Element] )( Set(_) );
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
        val child = db.dereference( extension.child );
        child match {
          case Branch( _, maybe )          => maybe;
          case Extension( EmptySubkey, _ ) => aerr( s"We expect only Branch terminators, not empty Leafs, as terminators of Extension! extension-> ${extension}, child -> ${child}" )
          case _                           => None;
        }
      }
      def replacementForIncluding( v : V ) : NewElements = {
        // an extension's child can only be a branch, otherwise it should have been merged into a much longer extension or leaf
        val childBranch : Branch = {
          db.dereference( extension.child ) match {
            case branch : Branch => branch;
            case bad             => aerr( s"An extension's child should always be a branch, instead we found: ${bad}" );
          }
        }
        val updatedChildBranch     = childBranch.copy( mbValue = Some(v) );
        val updatedChildBranchSource = db.reference( updatedChildBranch );
        val updatedExtension       = extension.copy( child=updatedChildBranchSource );
        NewElements( Element( updatedExtension ),  Element( updatedChildBranchSource, updatedChildBranch ) )
      }
      def replacementOrDeletionForExcluding : Option[NewElements] = {

        def withoutBranchValue( oldBranchChild : Branch ) = {
          val kidPairs = indexKidPairs( oldBranchChild.children );

          if ( kidPairs.length > 1 ) {
            val newBranchChild = oldBranchChild.copy( mbValue=None );
            val newBranchChildSource = db.reference( newBranchChild );
            val currentExtension = extension.copy( child=newBranchChildSource )
            NewElements( Element( currentExtension ), Element( newBranchChildSource, newBranchChild ) )
          } else {
            // condense
            val kidPair = kidPairs.head; //there had better be precisely one kidPair here
            val kidLetter = alphabet( kidPair._1 );
            val kidSource = kidPair._2;

            val onceCondensedExtension : Extension = Extension( extension.subkey :+ kidLetter, kidSource );
            val fullyCondensedNode     : Node      = condenseDownward( onceCondensedExtension )

            NewElements( fullyCondensedNode )
          }
        }

        val child = db.dereference( extension.child );
        child match {
          case oldBranchChild @ Branch( _, Some( _ ) )  => Some( withoutBranchValue( oldBranchChild ) )
          case _                                        => None;
        }
      }
    }
    case class ExactBranch( branch : Branch, elements : List[Element], matchLetterIndex : Int ) extends Path with Exact {
      // Per ethereum spec, Branches must have a child Branch or empty-key Leaf as "terminator" if it is to be associated with a value
      def mbValue : Option[V] = {
        val childSource = branch.children( matchLetterIndex );
        assert( childSource != NodeSource.Empty, "In an ExactBranch, the last letter should be matched, so the child should be nonzero." );
        val child = db.dereference( childSource );
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
            val newChildLeafSource = db.reference( newChildLeaf );
            val newCurrentBranch = Branch( branch.children.updated( matchLetterIndex, newChildLeafSource ), branch.mbValue );
            NewElements( Element( newCurrentBranch ), Element( newChildLeafSource, newChildLeaf ) )
          }
          def _nonemptyChildLeafCase = {
            val ( IndexedSeq( firstLetter ), remainder ) = childLeaf.subkey.splitAt(1);
            val firstLetterIndex = alphabet.indexOf( firstLetter );
            val grandchildLeaf = Leaf( remainder, childLeaf.value );
            val grandchildLeafSource = db.reference( grandchildLeaf );
            val newChildBranch = Branch( EmptyBranchChildren.updated( firstLetterIndex, grandchildLeafSource ), Some(v) );
            val newChildBranchSource = db.reference( newChildBranch );
            val newCurrentBranch = Branch( branch.children.updated( matchLetterIndex, newChildBranchSource ), branch.mbValue );
            NewElements( Element( newCurrentBranch ), Set( Element( newChildBranchSource, newChildBranch ), Element( grandchildLeafSource, grandchildLeaf ) ) )
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
            val newChildBranchSource = db.reference( newChildBranch );
            val newCurrentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildBranchSource ) );
            NewElements( Element( newCurrentBranch ), Element( newChildBranchSource, newChildBranch ) )
          }
          def _multiLetterCase = {
            // we reorganize childExtension into a Branch, which terminates the current Branch with a value, and a new Extension child of the new child Branch
            val firstLetter = childExtension.subkey.head;
            val firstLetterIndex = alphabet.indexOf( firstLetter );
            val newGrandchildExtensionSubkey = childExtension.subkey.tail;
            val newGrandchildExtensionChild = childExtension.child;
            val newGrandchildExtension = Extension( newGrandchildExtensionSubkey, newGrandchildExtensionChild )
            val newGrandchildExtensionSource = db.reference( newGrandchildExtension );
            val newChildBranchChildren = EmptyBranchChildren.updated( firstLetterIndex, newGrandchildExtensionSource );
            val newChildBranch = Branch( newChildBranchChildren, Some( v ) );
            val newChildBranchSource = db.reference( newChildBranch );
            val newCurrentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildBranchSource ) );
            NewElements( Element( newCurrentBranch ), Set( Element( newChildBranchSource, newChildBranch ), Element( newGrandchildExtensionSource, newGrandchildExtension ) ) )
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
          val updatedChildBranchSource = db.reference( updatedChildBranch );
          val updatedMainBranch      = branch.copy( children=branch.children.updated( matchLetterIndex, updatedChildBranchSource ) );
          NewElements( Element( updatedMainBranch ), Element( updatedChildBranchSource, updatedChildBranch ) )
        }

        // method implementation -- replacementForIncluding
        val childSource = branch.children( matchLetterIndex );
        assert( childSource != NodeSource.Empty, s"To be an ExactBranch match, the branch should have consumed the match letter, so its hash should not be NodeSource.Empty: childSource -> ${childSource}" );
        val childNode = db.dereference( childSource );
        childNode match {
          case childLeaf      : Leaf      => handleChildLeaf( childLeaf );
          case childExtension : Extension => handleChildExtension( childExtension );
          case childBranch    : Branch    => handleChildBranch( childBranch );
          case Empty                      => aerr( s"A lookup of a nonzero hash should not return Empty! childSource -> ${childSource}" );
        }
      }

      def replacementOrDeletionForExcluding : Option[NewElements] = {

        def removeChild = Some( NewElements( reshapeChildDroppingBranch( branch, matchLetterIndex ) ) )

        def reshapeChildBranch( childBranch : Branch )  = {
          val newChild = reshapeValueDroppingBranch( childBranch );
          val newChildSource = db.reference( newChild );
          val currentBranch = branch.copy( children=branch.children.updated( matchLetterIndex, newChildSource ) )
          Some( NewElements( Element( currentBranch ), Element( newChildSource, newChild ) ) )
        }
        val potentiallyTerminatingChild = db.dereference( branch.children( matchLetterIndex ) );
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
        val remainderLeafSource = db.reference( remainderLeaf );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( remainder.head ), remainderLeafSource ), Some( v ) );
        val newBranchSource = db.reference( newBranch );
        val currentExtension = Extension( matched, newBranchSource );
        NewElements( Element( currentExtension ), Set( Element( newBranchSource, newBranch ), Element( remainderLeafSource, remainderLeaf ) ) )
      }
    }
    case class OvershotByExtension( extension : Extension, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        // we make a Branch to terminate the matched portion
        // if the remainder is larger than one, we have to replace it with an Extension, otherwise the Branch suffices

        val mbRemainderExtensionElement = if (remainder.length > 1) Some( Element( Extension( remainder.tail, extension.child ) ) ) else None;
        val branchChildSource = mbRemainderExtensionElement.fold( extension.child )( _.source );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( remainder.head ), branchChildSource ), Some(v) );
        val newBranchSource = db.reference( newBranch );
        val newBranchElement = Element( newBranchSource, newBranch );
        val currentExtension = Extension( matched, newBranchSource );
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
          val newChildLeafSource = db.reference( newChildLeaf );
          val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( newLetters.head ), newChildLeafSource ), Some( leaf.value ) );
          NewElements( Element( newBranch ), Element( newChildLeaf ) )
        }

        def nonEmptyLeafSubkeyCase = {
          val newLettersLeaf = Leaf( newLetters.tail, v );
          val newLettersLeafSource = db.reference( newLettersLeaf );
          val oldLetters = leaf.subkey;
          val oldLettersLeaf = Leaf( oldLetters.tail, leaf.value );
          val oldLettersLeafSource = db.reference( oldLettersLeaf );
          val newBranchChildren = EmptyBranchChildren
            .updated( alphabet.indexOf( newLetters.head ), newLettersLeafSource )
            .updated( alphabet.indexOf( oldLetters.head ), oldLettersLeafSource );
          val newBranch = Branch( newBranchChildren, None );
          NewElements( Element( newBranch ), Set( Element( oldLettersLeafSource, oldLettersLeaf ), Element( newLettersLeafSource, newLettersLeaf ) ) )
        }

        if ( leaf.subkey.isEmpty ) emptyLeafSubkeyCase else nonEmptyLeafSubkeyCase
      }
    }
    case class TruncatedAtBeginningOfExtension( extension : Extension, elements : List[Element], newLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val newLettersLeaf = Leaf( newLetters.tail, v );
        val newLettersLeafSource = db.reference( newLettersLeaf );
        val newLettersLeafElement = Element( newLettersLeafSource, newLettersLeaf );
        val oldLetters = extension.subkey;
        val mbOldLettersExtensionElement = if ( oldLetters.length > 1 ) Some( Element( Extension( oldLetters.tail, extension.child ) ) ) else None;
        val oldLettersBranchChild = mbOldLettersExtensionElement.fold( extension.child )( _.source );
        val newBranchChildren = EmptyBranchChildren
          .updated( alphabet.indexOf( newLetters.head ), newLettersLeafSource )
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
        val newLettersLeafSource = db.reference( newLettersLeaf );
        val joinBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( newLetters.head ), newLettersLeafSource ), Some( leaf.value ) );
        val joinBranchSource = db.reference( joinBranch );
        val currentExtension = Extension( matched, joinBranchSource );
        NewElements( Element( currentExtension ), Set( Element( joinBranchSource, joinBranch ), Element( newLettersLeafSource, newLettersLeaf ) ) )
      }
    }
    case class TruncatedWithinBranch( branch : Branch, elements : List[Element], branchLetterIndex : Int, extraLetters : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = {
        val extraLettersLeaf = Leaf( extraLetters, v );
        val extraLettersLeafSource = db.reference( extraLettersLeaf );
        def msg = s"Huh? Truncation within a branch means that trying to traverse our next letter hits an empty child! branchLetterIndex -> ${branchLetterIndex}, branch -> ${branch}";
        assert( branch.children( branchLetterIndex ) == NodeSource.Empty, msg );
        val newBranchChildren = branch.children.updated( branchLetterIndex, extraLettersLeafSource );
        val newBranch = Branch( newBranchChildren, branch.mbValue );
        NewElements( Element( newBranch ), Element( extraLettersLeafSource, extraLettersLeaf ) )
      }
    }
    case class EmptySubkeyAtNonemptySubkeyRootLeaf( rootLeaf : Leaf ) extends Path {
      def elements = Element( rootLeaf ) :: Nil;

      def replacementForIncluding( v : V ) : NewElements = {
        val childLeaf = Leaf( rootLeaf.subkey.tail, rootLeaf.value );
        val childLeafSource = db.reference( childLeaf );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( rootLeaf.subkey.head ), childLeafSource ), Some( v ) );
        NewElements( Element( newBranch ), Element( childLeafSource, childLeaf ) )
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
        val childExtensionSource = db.reference( childExtension );
        val newBranch = Branch( EmptyBranchChildren.updated( alphabet.indexOf( rootExtension.subkey.head ), childExtensionSource ), Some( v ) );
        NewElements( Element( newBranch ), Element( childExtensionSource, childExtension ) )
      }
    }
    case class EmptySubkeyAtRootBranch( rootBranch : Branch ) extends Path with Exact {
      def elements = Element( rootBranch ) :: Nil;

      def mbValue   : Option[V] = rootBranch.mbValue;

      def replacementForIncluding( v : V ) : NewElements = NewElements( rootBranch.copy( mbValue=Some(v) ) )

      def replacementOrDeletionForExcluding : Option[NewElements] = {
        def oneChildCase( childIndex : Int ) = {
          val subkeyLetter = alphabet( childIndex );
          val childSource = rootBranch.children( childIndex );
          val childNode = db.dereference( childSource );

          def branchChildCase = NewElements( Extension( IndexedSeq( subkeyLetter ), childSource ) );
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
      val (newRoot, newPath) = Path.updatePath( elements, newElements.head );
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
                  case extension : Extension => newElements.children.filter( extension.child == _.source );
                  case branch    : Branch    => newElements.children.filter( elem => branch.children.contains( elem.source ) );
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
      UpdatedPath( newPath, lastAndChildren, newRoot )
    }
  }
}
