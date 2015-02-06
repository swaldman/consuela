package com.mchange.sc.v1.consuela.trie;

import scala.reflect.ClassTag;

object AltPMTrie {
  sealed trait Node[+L,+V,+H] {
    def subkey   : IndexedSeq[L];
    def children : IndexedSeq[H];
    def value    : Option[V];
  }
  case class Branch[L,V,H]( val letter : Option[L], val children : IndexedSeq[H], val value : Option[V] ) extends Node[L,V,H] {
    def subkey = letter.fold( IndexedSeq.empty[L] )( l => IndexedSeq( l ) );
  }
  case class Extension[L,V,H]( val subkey : IndexedSeq[L], val child : H, val value : Option[V] ) extends Node[L,V,H] {
    def children = IndexedSeq( child );
  }
  case object Empty extends Node[Nothing,Nothing,Nothing] {
    def subkey   = IndexedSeq.empty[Nothing];
    def children = IndexedSeq.empty[Nothing];
    def value    = None;
  }

  type Database[L,V,H] = PMTrie.Database[Node[L,V,H],H]
}


/*
 * Concrete classes should use an early initializer to initialize the value earlyInit!
 * 
 * This implementation is rendered unfortunately more complex by a (perhaps unnecessary, unwise)
 * decision to support the empty sequence (of generic type L) as a valid key.
 */ 
trait AltPMTrie[L,V,H] extends PMTrie[L,V,H, AltPMTrie.Node[L,V,H]] {

  /*
   * First lets put some unwieldy bits from the companion object into more convenient forms
   */ 
  type Node = AltPMTrie.Node[L,V,H];
  type Branch = AltPMTrie.Branch[L,V,H];
  type Extension = AltPMTrie.Extension[L,V,H];
  type Database = AltPMTrie.Database[L,V,H];

  import AltPMTrie.Empty;

  val Branch = AltPMTrie.Branch;
  val Extension = AltPMTrie.Extension;

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

  // let's replace this with an optimistic scheme that checks 
  // for gc updates only after we've observed a missing hash 
  //
  // if we restore this, zero znd hash should use rawDb.
  //
  //val rawDb : Database = earlyInit._1;
  //def db = if ( rawDb.knowsRoot( root ) ) rawDb else throw new AltPMTrie.FutureDatabaseException( root );

  val db : Database = earlyInit._1;
  val root : H      = earlyInit._2; 
  val Zero : H      = db.Zero;

  def hash( node : Node ) : H = db.hash( node );

  def apply( key : IndexedSeq[L] ) : Option[V] = {
    path( key ) match {
      case exact : Path.Exact => exact( key );
      case _                  => None;
    }
  }
  def including( key : IndexedSeq[L], value : V ) : Trie[L,V] = {
    val updatedPath = path( key ).including( value );
    persistClone( updatedPath );
  }
  def excluding( key : IndexedSeq[L] ) : Trie[L,V] = {
    path( key ) match {
      case exact : Path.Exact => persistClone( exact.excluding );
      case _                  => this;
    }
  }
  def dumpTrie : Unit = {
    def dumpNode( h : H ) : Unit = {
      val node = db( h )
      println( s"${h} -> ${node}" );
      node.children.filter( _ != Zero ).foreach( dumpNode(_) );
    }
    dumpNode( root );
  }
  def pathAsString( key : IndexedSeq[L] ) : String = path( key ).toString

  private[this] def path( key : IndexedSeq[L] ) : Path = Path.build( key );

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

  private[this] def aerr( message : String ) : Nothing = throw new AssertionError( message );

  private[this] object Path {

    def build( key : IndexedSeq[L] ) : Path = { 
      //println( s"root->${root} db(root)->${db(root)}" ); 
      build( root, db( root ), key, Nil )
    }

    private[this] val EmptyKey = IndexedSeq.empty[L];

    private[this] def build( nodeHash : H, node : Node, searchSubkey : IndexedSeq[L], parents : List[Element] ) : Path = {
      node match {
        case branch : Branch => buildBranch( nodeHash, branch, searchSubkey, parents );
        case extension : Extension => buildExtension( nodeHash, extension, searchSubkey, parents );
        case Empty => {
          assert( nodeHash == Zero, s"Huh? We were asked to build a path from the Empty node, yet it's claimed hash is nonzero? [nodeHash -> ${nodeHash}, node -> ${node}]" );
          assert( parents == Nil, s"We are asking to build a path from an Empty node which claims to have parents. [nodeHash -> ${nodeHash}, node -> ${node}, parents -> ${parents}]" );
          buildEmpty( searchSubkey );
        }
      }
    }
    private[this] def buildEmpty( searchSubkey : IndexedSeq[L] ) : Path =  /* if ( searchSubkey.isEmpty ) Exact( Nil ) else */ Truncated( Nil, searchSubkey );

    private[this] def buildExtension( nodeHash : H, extension : Extension, searchSubkey : IndexedSeq[L], parents : List[Element] ) : Path = {

      def _elements = Element( nodeHash, extension ) :: parents;
      //def _emptyRoot : Path = if ( searchSubkey.isEmpty ) Exact( _elements ) else Truncated( _elements, searchSubkey );
      def _exact = Exact( _elements );
      def _truncatedAbove = { // extension is not a match at all for the search subkey, our parent node is the last partial match so we must truncate their
        Truncated( parents, searchSubkey )
      }
      def _emptyKeyInsertion = {
        Overshot( _elements, EmptyKey, extension.subkey );
      }
      def _emptyKeyRoot = {
        if ( hasChild( extension ) ) {
          val child = extension.child;
          build( child, db(child), searchSubkey, _elements )
        } else {
          _truncatedEmptyKeyRoot;
        }
      }
      def _truncatedEmptyKeyRoot = { // extension is an empty root, our path is a truncation at the current node but with no matching letters
        Truncated( _elements, searchSubkey )
      }
      def _nonemptyTrie : Path = {
        val commonPrefixLen = searchSubkey.zip( extension.subkey ).takeWhile( tup => tup._1 == tup._2 ).length;
        val ( searchSubkeyLen, nodeSubkeyLen ) = ( searchSubkey.length, extension.subkey.length );
        def _overshot = Overshot( _elements, searchSubkey, extension.subkey.drop( searchSubkeyLen ) );
        def _truncatedHere = { // extension is a prefix match of the search subkey, but incomplete and has no children
          Truncated( _elements, searchSubkey.drop( commonPrefixLen ) );
        }
        def _divergent = {
          val ( common, divergence ) = searchSubkey.splitAt( commonPrefixLen );
          Divergent( _elements, common, divergence );
        }
        def _shortSearch = {
          //println( s"_shortSearch ${commonPrefixLen} ${searchSubkeyLen}" )

          if ( commonPrefixLen == searchSubkeyLen )
            _overshot 
          else if ( commonPrefixLen > 0 )
            _divergent
          else 
            _truncatedAbove // with no match on our key, the path should stop at parents one level above
        }
        def _longSearch = {
          if ( commonPrefixLen == nodeSubkeyLen ) { // the search key fully matches the node's key but is longer 
            if ( hasChild( extension ) ) {
              build( extension.child, db( extension.child ), searchSubkey.drop( commonPrefixLen ), _elements ); 
            } else {
              _truncatedHere
            }
          } else if ( commonPrefixLen > 0 )
            _divergent;
          else
            _truncatedAbove
        }
        def _equalSearch = {
          if ( searchSubkey == extension.subkey )
            _exact;
          else if ( commonPrefixLen > 0 )
            _divergent
          else
            _truncatedAbove // with no match on our key, the path should stop at parents one level above
        }

        if (searchSubkeyLen < nodeSubkeyLen ) 
          _shortSearch;
        else if ( searchSubkeyLen > nodeSubkeyLen ) 
          _longSearch;
        else
          _equalSearch
      }

      // after all of those definitions, here is the actual buildExtension(...) method...

      ( extension.subkey, searchSubkey, parents ) match {
        case ( EmptyKey, EmptyKey, Nil ) => _exact;
        case ( EmptyKey, _, Nil )        => _emptyKeyRoot; //build( extension.child, db(extension.child), searchSubkey, _elements ); //_truncatedEmptyRoot;
        case ( EmptyKey, _, _ )          => aerr( s"Huh? Empty subkey permitted on an Extension only at root. [parents -> ${parents}]" );
        case ( _, EmptyKey, Nil )        => _emptyKeyInsertion;
        case ( _, EmptyKey, _ )          => aerr( s"EmptyKey should never be searchSubkey at non-root extension. Should've been exact match at prior node. [parents -> ${parents}]" );
        case  _                          => _nonemptyTrie;
      }
    }
    private[this] def buildBranch( nodeHash : H, branch : Branch, searchSubkey : IndexedSeq[L], parents : List[Element] ) : Path = {
      def _elements = Element( nodeHash, branch ) :: parents;
      def _exact = Exact( _elements );

      def _tryDownward( nextKey : IndexedSeq[L] ) : Path = {
        val nextLetter = nextKey.head;
        val nextLetterIndex = alphabet.indexOf( nextKey.head );
        val nextHash = branch.children( nextLetterIndex );
        if ( nextHash != Zero )
          build( nextHash, db( nextHash ), nextKey, _elements );
        else
          Truncated( _elements, nextKey );
      }

      def _overshotRoot = Overshot( _elements, EmptyKey, branch.subkey ); 

      // after all of those definitions, here is the actual buildBranch(...) method...

      ( branch.letter, searchSubkey, parents ) match {
        case ( None, EmptyKey, Nil )                       => _exact;              //we have a no-letter root branch asked to supply its own value
        case ( None, key, Nil )                            => _tryDownward( key ); // we are at a no-letter root branch, with more key to consume
        case ( Some(_), EmptyKey, Nil )                    => _overshotRoot        //we are at a with-letter root branch, asked to find the empty key. we've overshot
        case ( None, _, _)                                 => aerr( "A non-root branch must always represent a letter." );
        case ( _, EmptyKey, _ )                            => aerr( "EmptyKey should never be searchSubkey at non-root branch. Should've been exact match at prior node." );
        case ( Some( l ), IndexedSeq( m ), _ ) if (l == m) => _exact;
        case ( Some( l ), key, _ ) if (l == key.head)      => _tryDownward( key.tail );
        case ( Some( _ ), key, _ )                         => Truncated( parents, key ) //since our letter doesn't match the first letter of the key, our best match was the parent node
      }
    }
    object Element {
      val Root     = Element( Zero, Empty );
      val Deletion = Element( Zero, null );

      def apply( node : Node ) : Element = Element( db.hash( node ), node );
    }
    case class Element( hash : H, node : Node );

    // note that the head element of a NewElements becomes the last element in modifiedPath in an UpdatedPath
    case class NewElements( head : Element, children : Set[Element] = Set.empty) {
      def all : Set[Element] = children + head;
    }
    case class UpdatedPath( modifiedPath : List[Element], lastAndChildren : Option[NewElements] ) { /* Note that lastAndChildren include children of the updated path not in elements */
      assert( 
        lastAndChildren == None || lastAndChildren.get.head == modifiedPath.head, 
        s"The head of lastAndChildren should be the leaf element of or updated path. [modifiedPath -> ${modifiedPath}, lastAndChildren -> ${lastAndChildren}]" 
      );
      def all : Set[Element] = lastAndChildren.fold( modifiedPath.toSet )( _.children ++ modifiedPath ); 
      def newRoot : Option[Element] = if ( modifiedPath == Nil ) None else Some(modifiedPath.last);
    }

    private def updatePath( oldPath : List[Element], newLastElement : Element ) : List[Element] = {
      def _singleNodePath = newLastElement :: Nil
      def _updateMultipleElementPath : List[Element] = {
        // reversedAccum builds with head closest to root and tail at or past leaf, opposite to how
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

      // after all of those definitions, here is the actual buildExtension(...) method...

      ( newLastElement, oldPath ) match {
        case ( Element.Deletion, Nil )      => aerr("Can't delete from an empty path!");
        case ( _, Nil )                     => _singleNodePath;
        case ( Element.Deletion, _ :: Nil ) => Nil
        case ( _, _ :: Nil )                => _singleNodePath;
        case _                              => _updateMultipleElementPath;
      }
    }

    case class Divergent( val elements : List[Element], commonPrefix : IndexedSeq[L], newDivergence : IndexedSeq[L] ) extends Path {
      assert( 
        {
          val cpLen = commonPrefix.length;
          elements.head match {
            case Element( _, Extension( subkey, _, _ ) ) => subkey.length > cpLen;
            case _                                       => false;
          }
        },
        s"Divergent paths should end with Extensions whose desired key is a partial (non-zero-length) prefix match of the last path element. ${this}"
      );
      def including( value : V ) : UpdatedPath = {
        def _newBranchForSplittableExtension( extension : Extension ) : NewElements = {
          val oldValueTail = Extension( extension.subkey.drop( commonPrefix.length ), extension.child, extension.value );
          val newValueTail = Extension( newDivergence, Zero, Some( value ) );
          val oldValueTailHash = db.hash( oldValueTail );
          val newValueTailHash = db.hash( newValueTail );
          val oldValueTailIndex = alphabet.indexOf( oldValueTail.subkey.head );
          val newValueTailIndex = alphabet.indexOf( newValueTail.subkey.head );
          val newBranchChildren = Path.branchChildren( oldValueTailIndex -> oldValueTailHash, newValueTailIndex -> newValueTailHash );
          val newBranchLetter = commonPrefix.last;
          val newBranch = Branch( Some( newBranchLetter ), newBranchChildren, None );
          NewElements( Element( newBranch ), Set( Element( oldValueTailHash, oldValueTail ), Element( newValueTailHash, newValueTail ) ) )
        }
        val commonPrefixLen = commonPrefix.length;
        val newElements = {
          elements match {
            case Element( _, extension @ Extension( _, _, _ ) ) :: _ => { 
              if (commonPrefixLen == 1)
                _newBranchForSplittableExtension( extension );
              else if ( commonPrefixLen > 1 ) {
                val newBranchNewElements = _newBranchForSplittableExtension( extension );
                val headExtension = Extension( commonPrefix.init, newBranchNewElements.head.hash, None );
                NewElements( Element( headExtension ), newBranchNewElements.all )
              } else {
                aerr( s"Bad Divergent path. Must be at least one, commonPrefixLen -> ${commonPrefixLen}" );
              }
            }
            case unexpected @ _ => aerr( s"Unexpected form of exact path: ${unexpected}" );
          }
        }
        updatedPath( newElements )
      }
    }
    case class Exact( val elements : List[Element] ) extends Path {
      assert( elements != Nil, "Even with an empty search key, we consider a Nil path to be truncated, not exact. Exact would be an empty subkey node." );
      def including( value : V ) : UpdatedPath = {
        // the easiest case, we just change the value of whatever we find
        val newElements = elements match {
          case Element( _, Branch( letter, children, _ ) ) :: _ => NewElements( Element( Branch( letter, children, Some( value ) ) ) );
          case Element( _, Extension( subkey, child, _ ) ) :: _ => NewElements( Element( Extension( subkey, child, Some( value ) ) ) );
          case unexpected @ _                                   => aerr( s"Unexpected form of exact path: ${unexpected}" );
        }
        updatedPath( newElements )
      }
      def excluding : UpdatedPath = {
        val Element( _, currentLastNode ) = elements.head;
        currentLastNode match {
          case Branch( letter, children, _ ) => updatedPath( NewElements( Element( Branch( letter, children, None ) ) ) ); // no structural changes from eliminating the value from a branch
          case Extension( subkey, Zero, _ )  => updatedPathForDeletion;
          case Extension( subkey, child, _ ) => updatedPath( attemptCondenseToNewElements( Extension( subkey, child, None ) ) );
          case Empty                         => aerr( s"Huh? EmptyNode should never be a match to any key. ${this}" );
        }
      }
      def apply( key : IndexedSeq[L] ) : Option[V]     = elements.head.node.value;
    }
    // at root, we can overshoot an empty String, for Branches or Extensions. Elsewhere, only Extensions can overshoot
    case class Overshot( val elements : List[Element], desiredSubkey : IndexedSeq[L], remainderSubkey : IndexedSeq[L] ) extends Path {
      def including( value : V ) : UpdatedPath = {
        def _overshotBranch( branchHash : H, l : L ) : NewElements = {
          assert(
            desiredSubkey == EmptyKey && remainderSubkey == IndexedSeq( l ),
            s"A branch can be an overshoot only at root, when empty is the desired key, and can only overshoot by one letter. [desiredSubkey -> ${desiredSubkey}, remainderSubkey -> ${remainderSubkey}]"
          )
          val newExtension = Extension( EmptyKey, branchHash, Some( value ) ); // Note that the branch itself is not a new child
          NewElements( Element( newExtension ) );
        }
        def _overshotExtension( oldSubkey : IndexedSeq[L], oldChild : H, oldValue : Option[V] ) : NewElements = {
          assert(
            oldSubkey == desiredSubkey ++ remainderSubkey,
            s"oldSubkey should equal desiredSubkey + remainderSubkey, [oldSubkey -> ${oldSubkey}, desiredSubkey -> ${desiredSubkey}, remainderSubkey -> ${remainderSubkey}]"
          )
          val rearExtension = Extension( remainderSubkey, oldChild, oldValue );
          val rearExtensionHash = db.hash( rearExtension );
          val frontExtension = Extension( desiredSubkey, rearExtensionHash, Some( value ) );
          NewElements( Element( frontExtension ), Set( Element( rearExtensionHash, rearExtension ) ) );
        }
        val newElements : NewElements = {
          elements match {
            case Element( branchHash, Branch(Some(l), _, _) ) :: Nil           => _overshotBranch( branchHash, l );
            case Element( _, Extension( oldSubkey, oldChild, oldValue ) ) :: _ => _overshotExtension( oldSubkey, oldChild, oldValue );
            case unexpected @ _                                                => aerr( s"Unexpected form of overshot path: ${unexpected}" );
          }
        }
        updatedPath( newElements )
      }
    }
    case class Truncated( val elements : List[Element], val unhandled : IndexedSeq[L] ) extends Path {
      assert(
        unhandled.length > 0 || elements == Nil,
        s"If there are no unhandled characters, this should have been an exact match rather than a trucation. [unhandled=${unhandled}, elements.length=${elements.length}]"
      );
      def including( value : V ) : UpdatedPath = {
        /**
        *  a new last element, merging a naive value-carrying Extension with the old last element
        */  
        val newElements : NewElements = {
          val naiveValueHolder = Extension( unhandled, Zero, Some( value ) );
          val nvhHash = db.hash( naiveValueHolder );
          val nvhElement = Element( nvhHash, naiveValueHolder );

          def insertRootBranch = {
            val rootNode = db( root );
            assert( rootNode.subkey.length > 0, "Path should not have been truncated at Nil if root node was empty String. [rootNode -> ${rootNode}]" );
            val rootNodeFirstLetter = rootNode.subkey.head;
            val rootNodeFirstLetterIndex = alphabet.indexOf( rootNodeFirstLetter );
            val newExtensionFirstLetter = naiveValueHolder.subkey.head;
            val newExtensionFirstLetterIndex = alphabet.indexOf( newExtensionFirstLetter );
            val children = branchChildren( rootNodeFirstLetterIndex -> root, newExtensionFirstLetterIndex -> nvhHash )
            val rootBranch = Branch( None, children, None );
            NewElements( Element( rootBranch ), Set( Element( nvhHash, naiveValueHolder ), Element( root, rootNode ) ) )
          }

          elements match {
            case Element( _, Extension( EmptyKey, _, None ) )                :: Nil => aerr( "An EmptyKey node should exist only at root and only when the EmptyKey is associated with a value." )
            case Element( _, Extension( EmptyKey, Zero, lastValue ) )        :: Nil => attemptCondenseToNewElements( Extension( EmptyKey, nvhHash, lastValue ), Some(nvhElement) );
            case Element( _, Extension( EmptyKey, lastChild, lastValue ) )   :: Nil => splitToBranched( EmptyKey, lastChild, lastValue, nvhHash, naiveValueHolder );
            case Element( _, Extension( EmptyKey, _, _ ) )                   :: _   => aerr( "EmptyKey is only a valid subkey on an element at root!" );
            case Element( _, Extension( lastSubkey, Zero, lastValue ) )      :: _   => attemptCondenseToNewElements( Extension( lastSubkey, nvhHash, lastValue ), Some(nvhElement) );
            case Element( _, Extension( lastSubkey, lastChild, lastValue ) ) :: _   => splitToBranched( lastSubkey, lastChild, lastValue, nvhHash, naiveValueHolder );
            case Element( _, Branch( lastLetter, lastChildren, lastValue ) ) :: _   => augmentedBranch( lastLetter, lastChildren, lastValue, nvhHash, naiveValueHolder );
            case Nil if (root == Zero)                                              => NewElements( Element( nvhHash, naiveValueHolder ) );
            case Nil                                                                => insertRootBranch;
            case unexpected @ _                                                     => aerr( s"Unexpected path form: ${unexpected}" );
          }
        }
        updatedPath( newElements )
      }
    }
    private def branchChildren( bindings : Tuple2[Int,H]* ) = {
      val array = Array.fill( alphabet.length )( Zero );
      bindings.foreach( binding => array( binding._1 ) = binding._2 );
      IndexedSeq( array : _* );
    }
    /* 
     * Note that we include the argument extension in NewElements object; it has not been and will need to be persisted.
     */
    def attemptCondenseToNewElements( extension : Extension, knownChild : Option[Element] = None ) : NewElements = {
      val condensed : Extension = attemptCondense( extension, knownChild );
      val condensedHash = db.hash( condensed );
      knownChild.fold( NewElements( Element( condensedHash, condensed ) ) ) { innerChildElement =>
        if ( condensed.child == innerChildElement.hash )
          NewElements( Element( condensedHash, condensed ), Set( innerChildElement ) )
        else
          NewElements( Element( condensedHash, condensed ) )
      }
    }
    /*
     * Note that we include newExtension in NewElements object; it has not been and will need to be persisted.
     * 
     * Here we are converting an Extension to a branch at its last letter, and which maybe (but maybe not) require
     * creating an additional head extension (depending on whether the subkey of the original Extension contained
     * more than a single letter.
     */ 
    private[this] def splitToBranched( lastKey : IndexedSeq[L], lastChild : H, lastValue : Option[V], newExtensionHash : H, newExtension : Extension ) : NewElements = {
      val branchChildren = {
        val newChildLetter = newExtension.subkey(0);
        val newChildLetterIndex = alphabet.indexOf( newChildLetter );
        val lastChildNode = db( lastChild );
        val oldChildLetter = lastChildNode.subkey(0);
        val oldChildLetterIndex = alphabet.indexOf( oldChildLetter );

        assert( 
          oldChildLetterIndex != newChildLetterIndex, 
          s"Should be truncated path: We should only split to branched if a former Extension will now have two distinct children. ${oldChildLetterIndex} ${newChildLetterIndex}" 
        );

        Path.branchChildren( newChildLetterIndex -> newExtensionHash, oldChildLetterIndex -> lastChild )
        /*
        val array = Array.fill( alphabet.length )( Zero );
        array( newChildLetterIndex ) = newExtensionHash;
        array( oldChildLetterIndex ) = lastChild;
        IndexedSeq( array : _* );
        */ 
      }
      val ( frontExtensionKey, branchKey ) : (IndexedSeq[L], Option[L]) = {
        lastKey match {
          case EmptyKey        => ( EmptyKey, None );
          case IndexedSeq( l ) => ( EmptyKey, Some( l ) );
          case _               => ( lastKey.init, Some( lastKey.last ) );
        }
      }
      val branch = Branch( branchKey, branchChildren, lastValue );
      val branchHash = db.hash( branch );

      val branchElement = Element( branchHash, branch );
      val newExtensionElement = Element( newExtensionHash, newExtension );

      if ( frontExtensionKey.isEmpty ) {
        NewElements( branchElement, Set( newExtensionElement ) );
      } else {
        val frontExtension = Extension( frontExtensionKey, branchHash, None )
        val frontExtensionHash = db.hash( frontExtension );
        NewElements( Element( frontExtensionHash, frontExtension ), Set( branchElement, newExtensionElement ) );
      }
    }
    /*
     * Note that we include newChildExtension in NewElements object; it has not been and will need to be persisted.
     */ 
    private[this] def augmentedBranch( lastLetter : Option[L], lastChildren : IndexedSeq[H], lastValue : Option[V], newChildExtensionHash : H, newChildExtension : Extension ) : NewElements = {
      val branchChildren = {
        val newChildLetter = newChildExtension.subkey(0);
        val newChildLetterIndex = alphabet.indexOf( newChildLetter );

        assert( lastChildren( newChildLetterIndex ) == Zero, "Truncated path: No prefix of the unhandled subkey should correspond to any child nodes." );

        val array = Array( lastChildren : _* );
        array( newChildLetterIndex ) = newChildExtensionHash;
        IndexedSeq( array : _* );
      }
      val newBranch = Branch( lastLetter, branchChildren, lastValue );
      val newBranchHash = db.hash( newBranch );
      NewElements( Element( newBranchHash, newBranch ), Set( Element( newChildExtensionHash, newChildExtension ) ) )
    }
    private[this] def newParent( newChild : Element, oldChild : Element, oldParent : Path.Element ) : Element = {
      def naiveNewParentBranch( oldParentBranch : Branch ) : Branch = {
        assert( oldParentBranch.children.contains( oldChild.hash ), "The old parent Branch node did not have the old child node for its child!" );
        def replaceIfOldChild( check : H, newChildHash : H, oldChildHash : H ) = if ( check == oldChildHash ) newChildHash else check;
        Branch( oldParentBranch.letter, oldParentBranch.children.map( replaceIfOldChild( _, newChild.hash, oldChild.hash ) ), oldParentBranch.value );
      }
      def naiveNewParentExtension( oldParentExtension : Extension ) : Extension = {
        assert(
          oldParentExtension.child == oldChild.hash,
          s"The old parent Extension node did not have the old child node for its child! [oldChild -> ${oldChild}, oldParentExtension.child -> ${oldParentExtension.child}]"
        );
        oldParentExtension.copy( child = newChild.hash )
      }
      def newParentFromExtension( oldParentExtension : Extension ) : Element = {
        assert(
          oldParentExtension.child == oldChild.hash,
          s"The old parent Extension node did not have the old child node for its child! [oldChild -> ${oldChild}, oldParentExtension -> ${oldParentExtension}]"
        );
        newChild match {
          case Path.Element.Deletion => 
            oldParentExtension match {
              case Extension(_, _, None) => Path.Element.Deletion; // our child is deleted, and we have no value, so we are deleted too.
              case Extension( subkey, child, value @ Some(_) ) => toElement( Extension( subkey, Zero, value ) );
            }
          case _ => {
            val naiveExtension = naiveNewParentExtension( oldParentExtension );
            toElement( attemptCondense( naiveExtension, Some( newChild ) ) ) // the child is known to be newChild here
          }
        }
      }
      def newParentFromBranch( oldParentBranch : Branch ) : Element = {
        val naiveReplacement = naiveNewParentBranch( oldParentBranch );
        childCount( naiveReplacement ) match {
          case 0 => aerr( s"Huh? We must have started with an illegal Branch with just one child, as we would have replaced at most one child. [oldParentBranch -> ${oldParentBranch}]" );
          case 1 => toElement( extensionFromOneChildBranch( naiveReplacement ) );
          case _ => toElement( naiveReplacement ); // we still have at least two children, so we must remain a branch
        }
      }
      def extensionFromOneChildBranch( branch : Branch ) : Extension = {
        assert( newChild == Path.Element.Deletion, s"To go from a branch to an extension, we must have deleted, but newChild is not the Deletion token? [newChild -> ${newChild}]");
        def uniqueChild( branch : Branch ) : H = {
          def uniqueChildIndex( children : IndexedSeq[H] ) : Int = {
            val childIndexPairs = children.zip( Stream.from(0) ).filter( _._1 != Zero );
            childIndexPairs.length match {
              case 1 => childIndexPairs(0)._2;
              case _ => aerr( s"children [${children}] did not contain one unique child, as required." );
            }
          }
          branch.children( uniqueChildIndex( branch.children ) );
        }
        val branchChildHash = uniqueChild( branch );
        val naiveExtension = Extension( branch.subkey, branchChildHash, branch.value );
        attemptCondense( naiveExtension ) // note that there is no known child here, as we've deleted one old child leaving some arbitrary orphan
      }

      // after all of those definitions, here is the actual newParent(...) method...

      oldParent.node match {
        case branch : Branch => newParentFromBranch( branch );
        case extension : Extension => newParentFromExtension( extension );
        case Empty => aerr(s"Huh? The Empty node should never show up as anybody's parent [newChild->${newChild}, oldChild->{oldChild}, oldParent->${oldParent}]");
      }
    }

    // will we use the functions below elsewhere? or should we nest them into newParent?
    private[this] def hasChild( extension : Extension ) = extension.child != Zero;
    private[this] def toElement( node : Node ) = Path.Element( db.hash( node ), node );
    private[this] def childCount( branch : Branch ) : Int = childCount( branch.children );
    private[this] def childCount( children : IndexedSeq[H] ) : Int = children.foldLeft(0)( (count, hash) => count + (if (hash == Zero) 0 else 1) );

    /*
     * Note that this method never creates a new child, so we don't have to bother
     * with returning a NewElements object.
     */
    private[this] def attemptCondense( parent : Extension, knownChild : Option[Element] = None ) : Extension = {
      def attemptCondenseExtension( parent : Extension, child : Extension ) : Option[Extension] = {
        if ( parent.value == None ) // we can condense, no value needs hang off an intermediate subkey
          Some( Extension( parent.subkey ++ child.subkey, child.child, child.value ) )
        else
          None;
      }
      def attemptCondenseNode( parent : Extension, child : Node ) : Option[Extension] = {
        child match {
          case _ : Branch => None; // if the child is a branch, we can't condense the pair into an Extension
          case kid : Extension => attemptCondenseExtension( parent, kid );
          case Empty => aerr(s"Huh? The Empty node should never show up as anybody's child [parent->${parent}, child->{child}]");
        }
      }

      val childElement = {
        knownChild.getOrElse{
          val childHash = parent.child;
          val childNode = db( childHash ); // if this blows up, you probably needed to supply a knownChild!
          Element( childHash, childNode )
        }
      }
      attemptCondenseNode( parent, childElement.node ).getOrElse( parent ); //if we can't condense with the child, we just return the parent, linked to the child
    }
    private[this] def isLeaf( element : Element ) : Boolean = {
      element match {
        case Element.Deletion => false;
        case Element( _, node ) => isLeaf( node );
      }
    }
    private[this] def isLeaf( node : Node ) : Boolean = {
      node match {
        case _ : Branch => false; 
        case Extension( _, child, _) if child != 0 => false;
        case Extension( _, child, Some(_)) if child == 0 => true;
        case Extension( _, child, None) if child == 0 => aerr(s"An extension with no child or value is useless, and should never come to exist [node -> ${node}]");
        case Empty => aerr(s"Huh? The Empty node should never show where it might be mistaken for a leaf!")
      }
    }
  }
  private[this] sealed trait Path {
    import Path._;

    def elements : List[Element];             // head is closest to leaf, last is root
    def including( value : V ) : UpdatedPath; // newElements not yet persisted, head is closest to leaf, last is proposed new root

    def updatedPath( newElements : NewElements ) : UpdatedPath = {
      val newPath = updatePath( elements, newElements.head );
      UpdatedPath( newPath, Some( newElements ) )
    }
    def updatedPathForDeletion : Path.UpdatedPath = {
      val newPath = Path.updatePath( elements, Element.Deletion );
      UpdatedPath( newPath, None )
    }
  }
}
