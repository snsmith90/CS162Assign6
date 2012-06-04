package cs162.miniJS.domains

import scala.io._
import scala.collection.mutable.{ Map => MMap, Buffer }
import cs162.miniJS.syntax._
import cs162.miniJS.values._
import Value._
import Domains._

object Domains {
  
  type Ref = Int

  // exception for impossible things
  object inconceivable extends Exception
  
  // exception for undefined behavior
  object undefined extends Exception

  // take a sequence of Values guaranteed to be Storables and downcast it
  def storable( vs:Seq[Value] ): Seq[Storable] =
    vs map (_.asInstanceOf[Storable])
  
    // take a Storable guaranteed to be an ObjectV and downcast it
  def toObj( v:Storable ): ObjectV =
    v match {
      case obj:ObjectV ⇒ obj
      case _ ⇒ throw inconceivable
    }
  
  // Functions to handle one corner case!
  def insertToObj(adr:Address, fld:StrV, v:Storable) :Unit = {
    val o = toObj( gStore.read(adr) )
    val location = o.getCells(fld)
    val head = o.head
    
    val dRef1 = addDummyRefernce(adr)
    val dRef2 = addDummyRefernce(head)
    val dRef3 = v match {
      case a:Address => addDummyRefernce(a)
      case _ => -1
    }
    
    location match {
      case None => {
        val newHead = gStore.alloc(ListCons( ( fld, v ), head ) )
        val newObj = toObj( gStore.read(adr) )
        newObj.head = newHead
        newObj.flushToHeap()
      }
      case Some( ( None, cell ) ) => {
        val next = gStore.read( cell ).asInstanceOf[ ListCons ].next
        val newHead = gStore.alloc(ListCons( ( fld, v ), next ) )
        val newObj = toObj( gStore.read(adr) )
        newObj.head = newHead
        newObj.flushToHeap()
      }
      case Some( ( Some( prev ), cur ) ) => {
        val next = gStore.read( cur ).asInstanceOf[ ListCons ].next
        val newCell = gStore.alloc( ListCons( ( fld, v ), next) )
        val prevCons = gStore.read( prev ).asInstanceOf[ ListCons ]
        prevCons.next = newCell
        prevCons.flushToHeap()
      }
    }
    
    gEnv.pop()
    gEnv.pop()
    gStore.refMap -= dRef1
    gStore.refMap -= dRef2
    
    if (dRef3 != -1) {
      gEnv.pop()
      gStore.refMap -= dRef3
    }
  }
  
  def addDummyRefernce(a:Address): Ref = {
    val ref = FreshRef.getNext
    gStore.refMap += (ref -> a)
    gEnv push (FreshVar.getNext() -> ref)
    ref
  }
  def createNewObj(fs:Seq[String], vs:Seq[Storable]) : ObjectV = {
    val newObj = ObjectV()
    
    (fs zip vs).foreach((pair) => {
      val (fld, v) = pair
      val location = newObj.getCells(fld)
      val head = newObj.head
    
      location match {
        case None => {
          val newHead = gStore.alloc(ListCons( ( fld, v ), head ) )
          newObj.head = newHead
        }
        case Some( ( None, cell ) ) => {
          val next = gStore.read( cell ).asInstanceOf[ ListCons ].next
          val newHead = gStore.alloc(ListCons( ( fld, v ), next ) )
          newObj.head = newHead
        }
        case Some( ( Some( prev ), cur ) ) => {
          val next = gStore.read( cur ).asInstanceOf[ ListCons ].next
          val newCell = gStore.alloc( ListCons( ( fld, v ), next) )
          val prevCons = gStore.read( prev ).asInstanceOf[ ListCons ]
          prevCons.next = newCell
          prevCons.flushToHeap()
        }
      }
    })
    
    newObj
  }
    
}

// global store
object gStore {

  import cs162.miniJS.gc._
  
  val refMap: MMap[Ref, Address] = MMap()
  var listNilAddress = Address(-1)
  var listNilRef:Ref = -1
  
  var HEAP_SIZE = 1024 * 400
  var gc: Collector = new StubCollector( HEAP_SIZE )

  def initStore(heapSize:Int, gCollector:Collector) = {
    HEAP_SIZE = heapSize
    gc = gCollector
    
    //Initialize the listNilAddress
    listNilAddress = alloc(ListNil)
    listNilRef = FreshRef.getNext 
    refMap += (listNilRef -> listNilAddress)
    gEnv.push(FreshVar.getNext(), listNilRef)
    
    gEnv.push( "gObj" -> ( gStore += ObjectV() ) )
  }
  
  def apply( r:Ref ): Storable = {
    refMap get r match {
      case Some(a) => gc.gcRead(a)
      case _       => throw undefined
    }
  }
  
  def read (a: Address): Storable = {
    gc.gcRead(a)
  }

  def +=( v: Storable ): Ref = {
    val adr = alloc(v)
    val ref = FreshRef.getNext
    refMap += (ref -> adr)
    ref
  }
  
  def alloc( v: Storable ): Address = {
    // EDGE CASE
    // we garbage collect while allocating a cons cell
    // it is possible that nothing in the environment will point to these
    v match {
      case ListCons( ( _, a1: Address), a2 ) => {
        val ref1 = FreshRef.getNext
        val ref2 = FreshRef.getNext
        gEnv.push( "1" -> ref1 )
        gEnv.push( "2" -> ref2 )
        refMap += (ref1 -> a1)
        refMap += (ref2 -> a2)
        val retval = gc.gcAlloc( v )
        gEnv.pop()
        gEnv.pop()
        refMap -= ref1
        refMap -= ref2
        retval
      }
      case ListCons( _, a ) => {
        val ref1 = FreshRef.getNext
        gEnv.push( "1" -> ref1 )
        refMap += (ref1 -> a)
        val retval = gc.gcAlloc( v )
        gEnv.pop()
        refMap -= ref1
        retval
      }
      case _ =>
        gc.gcAlloc( v )
    }
  }

  def ++=( vs: Seq[ Storable ] ): Seq[ Ref ] = 
    vs.map( this += _ )

  // simulate an update by allocating new room and changing
  // the old address to use this old room
  def update( r: Ref, v: Storable ): Storable = {
    val newAddress = alloc(v)
    gc.updateAddress( refMap(r), newAddress )
    UndefV()
  }
}

import scala.collection.mutable.{ Stack => SStack }
case class Env( env: SStack[ ( String, Ref ) ] = new SStack() ) {
  def apply( x: String ): Ref =
    env.find( _._1 == x ) match {
      case Some( ( _, a ) ) => a
      case _ => throw undefined
    }

  def push( binding: ( String, Ref ) ) {
    env.push( binding )
  }

  def pop() =
    env.pop()

  def ++( bindings: Seq[ ( String, Ref ) ] ) {
    bindings.foreach( push( _ ) )
  }
}

object gEnv extends Env() {
}

object FreshVar {
  private var id: BigInt = 4
  
  def getNext() = {id += 1; id.toString()}
}

object FreshRef {
  private var id: Int = 0
  
  def getNext = {id += 1; id}
}