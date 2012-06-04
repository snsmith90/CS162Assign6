package cs162.miniJS.values

import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import Value._
import Domains._

// language values
sealed abstract class Value

// companion object
object Value {

  // implicit conversions
  implicit def v2n( v:NumV )    : BigInt  = v.n
  implicit def v2b( v:BoolV )   : Boolean = v.b
  implicit def v2s( v:StrV )    : String  = v.s
  implicit def n2v( n:BigInt )  : NumV    = NumV( n )
  implicit def b2v( b:Boolean ) : BoolV   = BoolV( b )
  implicit def s2v( s:String )  : StrV    = StrV( s )
  
}

// storable values (non-exceptional)
sealed abstract class Storable extends Value {
  
  def T: Boolean

  def + ( v:Storable ): Storable
  def − ( v:Storable ): Storable
  def × ( v:Storable ): Storable
  def ÷ ( v:Storable ): Storable
  def ≈ ( v:Storable ): Storable
  def ≠ ( v:Storable ): Storable
  def ≤ ( v:Storable ): Storable
  def < ( v:Storable ): Storable
  def ∧ ( v:Storable ): Storable
  def ∨ ( v:Storable ): Storable
  
  def neg: Storable
  def not: Storable
}

// numbers
case class NumV( n:BigInt ) extends Storable {
  def T = n != 0
  
  def + ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n + _n
    case _ ⇒ throw undefined
  }
  def − ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n - _n
    case _ ⇒ throw undefined
  }
  def × ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n * _n
    case _ ⇒ throw undefined
  }
  def ÷ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ if ( _n != 0 ) n / _n else throw undefined
    case _ ⇒ throw undefined
  }
  def ≈ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n == _n
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n != _n
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n <= _n
    case _ ⇒ throw undefined
  }
  def < ( v:Storable ) = v match {
    case NumV( _n ) ⇒ n < _n
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = -n
  def not = !this.T
  
  override def toString = n.toString
}
  
// booleans
case class BoolV( b:Boolean ) extends Storable {
  def T = b
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case BoolV( _b ) ⇒ b == _b
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case BoolV( _b ) ⇒ b != _b
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = b.toString
}

// strings
case class StrV( s:String ) extends Storable {
  def T = s != ""
  
  def + ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s + _s
    case _ ⇒ throw undefined
  }
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s == _s
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s != _s
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s <= _s
    case _ ⇒ throw undefined
  }
  def < ( v:Storable ) = v match {
    case StrV( _s ) ⇒ s < _s
    case _ ⇒ throw undefined
  }
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = s
}

// undefined value
case class UndefV() extends Storable {
  def T = false
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case UndefV() ⇒ true
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case UndefV() ⇒ false
    case _ ⇒ true
  } 
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = "undef"
}

// closures
case class CloV( xs:Seq[String], t:Term, env:Env ) extends Storable {
  def T = true
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = false
  def ≠ ( v:Storable ) = true
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T
  
  override def toString = "[closure]"
}


// essentially, anything that can mutate
// these items, if changed, must also be updated on the heap
// in order to do this, we must know where they are allocated
// on the heap so we can overwrite them.
// IN ADDITION, anything extending this trait MUST
// have the following properties:
// 1.) Its size, in bytes, is constant no matter what
// 2.) It doesn't exist in multiple places on the heap
// 
// Assertion violations will result if the first property doesn't
// hold, and odd behavior will result if 2 doesn't hold, namely
// old versions will stick around
trait MayMutate {
  var whereAllocated = -1

  // signals that a change occurred on this object, and so it
  // must be flushed out
  def flushToHeap() = 
    gStore.gc.mutated( this )
}

// store locations
// this is now a var, for the following reasons:
// 1.) If we reassign, we will need to allocate new room elsewhere on the heap,
//     but the original address doesn't really change
// 2.) Copying collectors need to change this anyway
// Note this would break a typical store mapping of addresses -> values, since
// the location change wouldn't be reflected in the key.  However, the GC
// isn't a typical store mapping.  There is a _crucial_ invariant assumed
// here: For any given loc, there is at most one Address object associated with
// it.
//ρ.push(this +=
// -loc is the address that this address points to
// -whereAllocated is the address where the address itself is allocated (-1
//  indicates that it's not currently allocated)
case class Address( var loc:Int ) extends Storable with MayMutate {
  def T = true

  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = v match {
    case Address( _loc ) ⇒ loc == _loc
    case _ ⇒ false
  }
  def ≠ ( v:Storable ) = v match {
    case Address( _loc ) ⇒ loc != _loc
    case _ ⇒ true
  }
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T

  def neg = throw undefined
  def not = !this.T
  
  override def toString = "a" + loc 
}

// special linked-list implementation for maps
// this is needed to simplify garbage collection
// values cannot change, but next addresses can change
// INVARIANT: the only thing that can be shared between maps is
// ListNil
trait ListNode extends Storable {
  def value(): ( String, Storable )
  def next(): Address

  // override the rest of the Storable stuff
  def T = throw inconceivable

  def + ( v:Storable ) = throw inconceivable
  def − ( v:Storable ) = throw inconceivable
  def × ( v:Storable ) = throw inconceivable
  def ÷ ( v:Storable ) = throw inconceivable
  def ≈ ( v:Storable ) = throw inconceivable
  def ≠ ( v:Storable ) = throw inconceivable
  def ≤ ( v:Storable ) = throw inconceivable
  def < ( v:Storable ) = throw inconceivable
  def ∧ ( v:Storable ) = throw inconceivable
  def ∨ ( v:Storable ) = throw inconceivable
  
  def neg = throw inconceivable
  def not = throw inconceivable
}

case object ListNil extends ListNode {
  def value() = throw inconceivable
  def next() = throw inconceivable
}
case class ListCons( value: ( String, Storable ), var next: Address ) extends ListNode with MayMutate

case class ObjectV( var head: Address = gStore.listNilAddress ) extends Storable with MayMutate with java.io.Serializable {
  def T = true
  
  def + ( v:Storable ) = throw undefined
  def − ( v:Storable ) = throw undefined
  def × ( v:Storable ) = throw undefined
  def ÷ ( v:Storable ) = throw undefined
  def ≈ ( v:Storable ) = throw undefined
  def ≠ ( v:Storable ) = throw undefined
  def ≤ ( v:Storable ) = throw undefined
  def < ( v:Storable ) = throw undefined
  def ∧ ( v:Storable ) = this.T && v.T
  def ∨ ( v:Storable ) = this.T || v.T
  
  def neg = throw undefined
  def not = !this.T

  // field lookup using prototype-based inheritance
  def apply( s:String ): Storable = get( s ) match {
    case Some( v ) ⇒ v
    case None ⇒ get( "proto" ) match {
      case None ⇒ UndefV()
      case Some( a:Address ) ⇒ gStore.read( a ) match {
        case obj:ObjectV ⇒ obj( s )
        case _:Storable ⇒ throw inconceivable
        case except ⇒ except
      }
      case x ⇒ throw undefined
    }
  }
  
  def :+( sv:Tuple2[StrV,Storable] ) {
    insert( sv._1, sv._2 )
  }

  def toMap() = 
    Map() ++ toPairs

  def keys() =
    toPairs.map( _._1 )

  def values() =
    toPairs.map( _._2 )

  // gets all ( String, Storable ) pairs
  def toPairs() =
    toTriples.map( triple =>
      ( triple._1, triple._2 ) )

  // gets all cons cells as ( String, Storable, Address ) triples
  def toTriples() = {
    def toTriples( accum: List[ ( String, Storable, Address ) ], a: Address ): List[ ( String, Storable, Address ) ] =
      gStore.read( a ) match {
        case ListCons( ( key, value ), next ) =>
          toTriples( ( key, value, next ) :: accum, next )
        case ListNil => accum
        case _ => throw inconceivable
      }
    toTriples( List(), head )
  }

  override def toString() =
    toTriples.toString

  // gets the previous and current matching cells, or None
  def getCells( key: String ): Option[ ( Option[ Address ], Address ) ] = {
    def get( lastAddr: Option[ Address ], curAddr: Address ): Option[ ( Option[ Address ], Address ) ] =
      gStore.read( curAddr ) match {
        case ListNil => None
        case ListCons( ( `key`, value ), _ ) => Some( lastAddr, curAddr )
        case ListCons( _, n ) => get( Some( curAddr ), n )
        case _ => throw inconceivable
      }
    get( None, head )
  }

  def get( key: String ): Option[ Storable ] =
    getCells( key ).map( pair =>
      gStore.read( pair._2 ).asInstanceOf[ ListCons ].value._2 )

  def contains( key: String ) =
    get( key ).isDefined

  // inserts a mapping into this map
  def insert( key: String, value: Storable ) { 
    getCells( key ) match {
      case None => {
        // there isn't a mapping already
        // put it at the front, not touching the rest of the list
        head = ( gStore.alloc(ListCons( ( key, value ), head ) ))
        flushToHeap()
      }
      case Some( ( None, cell ) ) => {
        // there is a mapping at the front of the list
        // add a cell containing this mapping, and have it point to
        // the rest of the map
        head = ( gStore.alloc( ListCons( ( key, value ), 
                             gStore.read( cell ).asInstanceOf[ ListCons ].next ) ))
        flushToHeap()
      }
      case Some( ( Some( prev ), cur ) ) => {
        // there is a mapping in the middle of the list
        // add a cell with this mapping, having the previous
        // start pointing to it
        val newCell = ( gStore.alloc( ListCons( ( key, value ), 
                                       gStore.read( cur ).asInstanceOf[ ListCons ].next ) ))
        val prevCons = gStore.read( prev ).asInstanceOf[ ListCons ]
        prevCons.next = newCell
        prevCons.flushToHeap()
      }
    } // getCells
  } // insert
} // ObjectMap

