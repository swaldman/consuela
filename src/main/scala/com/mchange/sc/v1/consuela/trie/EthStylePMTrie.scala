package com.mchange.sc.v1.consuela.trie;

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

  val db       : Database = earlyInit._1;
  val Zero     : H      = db.Zero;
  val rootHash : H      = earlyInit._2;
  
  lazy val rootNode = db( rootHash );

  val alphabetLen = alphabet.length;

  // useful empties
  val EmptyBranchChildren = IndexedSeq.fill( alphabetLen )( Zero );
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
      case exact : Path.Exact => persistClone( exact.excluding );
      case _                  => this;
    }
  }

  def subkeys( branch : Branch ) : Seq[L] = branch.children.zip( Stream.from(0) ).filter( _._1 != Zero ).map( tup => alphabet( tup._2 ) );


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

  private[this] def path( key : Subkey ) : Path = Path.Builder.build( key );

  private[this] def aerr( message : String ) : Nothing = throw new AssertionError( message );

  object Path {
    object Builder {
      def build( key : IndexedSeq[L] ) : Path = {
        //println( s"rootHash->${rootHash} rootNode->${rootNode}" );
        build( rootHash, rootNode, key, Nil )
      }
      private def build( nodeHash : H, node : Node, searchSubkey : Subkey, parents : List[Element] ) : Path = {
        import SubkeyComparison._

        // build utilities
        def _fromEmpty( searchSubkey : Subkey ) : Path =  TruncatedAtRoot( searchSubkey );
        def _fromExtension( nodeHash : H, extension : Extension, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, extension.subkey );
          def nextElements = Element( nodeHash, extension ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnExtension )                 => OvershotByExtension( extension, nextElements, matched, unmatchedOnExtension );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => build( extension.child, db( extension.child ), unmatchedOnSubkey, nextElements );
            case MatchExact( matched )                                          => ExactExtension( extension, nextElements );
            case NoMatch                                                        => TruncatedAboveUnmatchableExtension( extension, parents, searchSubkey );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnExtension )  => DivergentExtension( extension, nextElements, matched, unmatchedOnExtension, unmatchedOnSubkey );
          }
        }
        def _fromLeaf( nodeHash : H, leaf : Leaf, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          val subkeyComparison = subkeyCompare( searchSubkey, leaf.subkey );
          def nextElements = Element( nodeHash, leaf ) :: parents;
          subkeyComparison match {
            case MatchLessThan( matched, unmatchedOnLeaf )                      => OvershotByLeaf( leaf, nextElements, matched, unmatchedOnLeaf );
            case MatchGreaterThan( matched, unmatchedOnSubkey )                 => TruncatedWithinLeaf( leaf, nextElements, matched, unmatchedOnSubkey );
            case MatchExact( matched )                                          => ExactLeaf( leaf, nextElements );
            case NoMatch                                                        => TruncatedAboveUnmatchableLeaf( leaf, parents, searchSubkey );
            case Divergent( matched, unmatchedOnSubkey, unmatchedOnLeaf )       => DivergentLeaf( leaf, nextElements, matched, unmatchedOnLeaf, unmatchedOnSubkey );
          }
        }
        def _fromBranch( nodeHash : H, branch : Branch, searchSubkey : Subkey, parents : List[Element] ) : Path = {
          def nextElements = Element( nodeHash, branch ) :: parents;
          val firstLetterIndex = alphabet.indexOf( searchSubkey.head );
          val childHash = branch.children( firstLetterIndex );
          childHash match {
            case Zero                                 => TruncatedWithinBranch( branch, nextElements, firstLetterIndex, searchSubkey.tail );
            case goodHash if searchSubkey.length == 1 => ExactBranch( branch, nextElements, firstLetterIndex );
            case goodHash                             => build( goodHash, db( goodHash ), searchSubkey.tail, nextElements );
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
      val Root     = Element( Zero, Empty );
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
    private def updatePath( oldPath : List[Element], newLastElement : Element ) : List[Element] = ???

    case class DivergentLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, oldRemainder : Subkey, newDivergence : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class DivergentExtension( extension : Extension, elements : List[Element], matched : Subkey, oldRemainder : Subkey, newDivergence : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class ExactLeaf( leaf : Leaf, elements : List[Element] ) extends Path with Exact {
      def mbValue : Option[V] = Some( leaf.value )

      // easy-peasy, no structural changes or children, just switch out the value
      def replacementForIncluding( v : V ) : NewElements = NewElements( leaf.copy( value=v ) )

      def replacementOrDeletionForExcluding : NewElements = ???;
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

      def replacementOrDeletionForExcluding : NewElements = ???;
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

      def replacementOrDeletionForExcluding : NewElements = ???;
    }
    case class OvershotByExtension( extension : Extension, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class OvershotByLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class TruncatedAtRoot( remainder : Subkey ) extends Path {
      val elements : List[Element] = Nil;

      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class TruncatedAboveUnmatchableLeaf( leaf : Leaf, elements : List[Element], val remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class TruncatedAboveUnmatchableExtension( extension : Extension, elements : List[Element], remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class TruncatedWithinLeaf( leaf : Leaf, elements : List[Element], matched : Subkey, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    case class TruncatedWithinBranch( branch : Branch, elements : List[Element], newBranchChildIndex : Int, remainder : Subkey ) extends Path {
      def replacementForIncluding( v : V ) : NewElements = ???
    }
    trait Exact {
      self : Path =>

      def mbValue   : Option[V]
      def excluding : UpdatedPath = updatedPath( replacementOrDeletionForExcluding );

      def replacementOrDeletionForExcluding : NewElements;
    }
  }
  sealed trait Path {
    import Path._;
    def elements  : List[Element]; // head is closest to leaf, last is root
    def replacementForIncluding( value : V ) : NewElements;

    def including( value : V ) : UpdatedPath = updatedPath( replacementForIncluding( value ) );

    def updatedPath( newElements : NewElements ) : UpdatedPath = {
      val newPath = Path.updatePath( elements, newElements.head );
      UpdatedPath( newPath, Some( newElements ) )
    }
    /*
    def updatedPathForDeletion : Path.UpdatedPath = {
      val newPath = Path.updatePath( elements, Element.Deletion );
      UpdatedPath( newPath, None )
    }
    */
  }
}
