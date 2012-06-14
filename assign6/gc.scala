package cs162.miniJS.gc

import cs162.miniJS.values._
import cs162.miniJS.domains.Domains._
import cs162.miniJS.domains.{ gEnv, gStore, FreshRef }
import scala.collection.mutable.{ Stack }
import scala.collection.immutable.{ Set }

// out of memory exception
case object OOM extends Exception( "Out of memory" )

// various utility functions useful for garbage collectors
object GCUtils {
  // convert an Int into 4 bytes
  def i2b(x:Int) : Array[Byte] = {
    val result = new Array[Byte](4)
    for (i <- 0 until 4) result(i) = ((x >>> ((3-i)*8)) & 0xff).toByte
    result
  }

  // convert 4 bytes into an Int
  def b2i(b:Array[Byte]) : Int = {
    var result = 0
    for (i <- 0 until 4) result += (b(i) & 0x000000ff) << ((3-i)*8)
    result
  }

  // convert an object to a byte array
  def toBytes( o: Object ) : Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream()
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o) ; out.flush() ; out.close()
    ba.flush() ; ba.toByteArray()
  }

  // get object size in bytes
  def getObjectSize(obj:Object) : Int = toBytes(obj).size
}

import GCUtils._

// Abstraction of a garbage collector
// Note that everything Storable can be garbage collected
// takes the number of bytes for the available memory space
abstract class Collector( val maxSize: Int ) {
  var tracep = true
  def trace(msg : => String) = if (tracep) println(msg)

  protected val heap = new Array[ Byte ]( maxSize )
  def gcAlloc( s: Storable ): Address
  def gcRead( a: Address ): Storable // Value since the address might not be valid
  def updateAddress( oldAddr: Address, newAddr: Address ): Unit
  def mutated( m: MayMutate ): Unit // flush out the given object

  // write a byte to the heap
  def writeByte(idx:Int, n:Byte) { heap(idx) = n }

  // read a byte from the heap
  def readByte(idx:Int) = heap(idx)

  // write a 32-bit integer to the heap
  def writeInt(idx:Int, n:Int) = i2b(n).copyToArray(heap, idx, idx+4)

  // read a 32-bit integer from the heap
  def readInt(idx:Int) : Int = b2i(heap.slice(idx, idx+4))

  // write a memory object to the heap
  def writeObject( idx:Int, o:Object ) {
    writeBytes( idx, toBytes( o ) )
  }

  def writeBytes( index: Int, bytes: Array[ Byte ] ) {
	trace("in writeBytes given index " + index + "and bytes " + bytes)
    bytes.copyToArray( heap, index, bytes.size )
  }

  // read a memory object (which is 'size' bytes long) from the heap
  def readObject(idx:Int, size:Int) : Object = {
	trace("In read obj")
    val bytes = heap.slice(idx, idx+size)
	//trace("Amt of bytes is " + bytes)
    val ba = new java.io.ByteArrayInputStream(bytes)
	//trace("Byte array " + ba)
    val in = new java.io.ObjectInputStream(ba)
    in.readObject()
  }

  def validAddress( a: Address ): Boolean =
    ( a.loc >= 0 && a.loc < maxSize )
}

// doesn't actually garbage collect
// once it's out of memory, it's out of memory
// ALLOCATED OBJECT FORMAT:
// -Size of object (4 bytes)
// -Object itself (n bytes, where n is the size)
class StubCollector( max: Int ) extends Collector( max ) {
  private var bumpPointer = 0 // where we are in the heap
  val metadataSize = 4

  def mutated( m: MayMutate ) {
    if ( m.whereAllocated != -1 ) {
      val asBytes = toBytes( m )
      trace( "## mutated: Flushing mutated object at " + m.whereAllocated )
      assert( readInt( m.whereAllocated ) >= asBytes.length )
      writeBytes( m.whereAllocated + 4, asBytes )
    }
  }

  // allocates a given amount of size
  // returns the index to the start of that place
  def allocSize( n: Int ): Int = {
    val postBump = bumpPointer + n
    if ( postBump >= maxSize )
      throw OOM
    else {
      val retval = bumpPointer
      bumpPointer = postBump
      retval
    }
  }
      
  def gcAlloc( s: Storable ): Address = {
    trace( "## gcAlloc: allocating space for " + s )
    var asBytes = toBytes( s )
    val index = allocSize( asBytes.size + metadataSize )
	trace("In stub gcAlloc this is storable" + s)
    s match {
      case m: MayMutate => {
        m.whereAllocated = index
        val newAsBytes = toBytes( m )
        assert( asBytes.length == newAsBytes.length )
        asBytes = newAsBytes ++ (0 until 50).map(_.toByte)
      }
      case _ => ()
    }
    writeInt( index, asBytes.size )
    writeBytes( index + 4, asBytes)

    trace( "## gcAlloc: allocated " + (asBytes.size + metadataSize) + 
           "bytes starting at addr " + index )

    Address( index )
  }

  def gcRead( a: Address ): Storable = {
    trace( "## gcRead: reading " + a )
    assert(validAddress( a ))
    
    val retval = readObject( a.loc + 4, readInt( a.loc ) ).asInstanceOf[ Storable ]
    trace( "## gcRead: result == " + retval )
    retval
  }

  def updateAddress( aOld: Address, aNew: Address ) {
    // overwrite the old address with the new one
    assert( aNew.whereAllocated == -1 )
    if ( aOld.whereAllocated != -1 ) {
      aNew.whereAllocated = aOld.whereAllocated
      val asBytes = toBytes( aNew )
      trace( "## updateAddress: Moving allocated address." ) 
      assert( readInt( aOld.whereAllocated ) == asBytes.length )
      writeBytes( aOld.whereAllocated + 4, asBytes )
    }
    aOld.loc = aNew.loc
  }
}

trait TracingCollector {
  // Gets the root set
  // You can get the entire root-set by traversing
  // 'gEnv', the global environment.

  def rootSet(): Set[ Ref ] = {
    // FILL ME
	//trace("In rootSet")
	val x = Set( FreshRef.getNext )
	x
  }

}

class SemispaceCollector( heapSize: Int ) extends Collector( heapSize ) with TracingCollector {
  //---------- PRIVATE FIELDS AND METHODS ----------
  val NOT_COPIED: Byte = 0
  val COPIED: Byte = 1
  val PADDING = 50
  val metadataSize = 5

  // Sanity check for heapSize
  if ( heapSize % 2 != 0 ) {
    throw cs162.miniJS.domains.Domains.undefined
  }

  // Initialization code
  var bumpPointer = 0
  var fromStart = 0
  var toStart = heapSize / 2

  // ALLOCATED OBJECT FORMAT:
  // -Size of the object (4 bytes) (NOT including metadata)
  // -Copied? (1 byte) (i.e. it's in the other heap)
  // -Rest of object
  
  // COPIED OBJECT FORMAT:
  // -Address of object (4 bytes)
  // -Copied? (True - 1 byte)
  // -junk
  
  // IMPORTANT NOTE: 
  //
  // (1) an object's address is the start of the actual object, *not*
  //     including the meta-data; the meta-data is at a negative
  //     offset from the object address
  //
  // (2) size field in the allocated object meta-data includes only
  //     the allocated object, *not* the meta-data itself

  
  // Convenience functions for reading an writing object metadata and data 
  def isCopied( i: Int ) = readByte( i + 4 ) == COPIED
  def setCopied( i: Int ) = writeByte( i + 4, COPIED )
  def setNotCopied( i: Int ) = writeByte( i + 4, NOT_COPIED )
  def readSize( i: Int ) = readInt( i )
  def setSize( i: Int, size: Int ) = writeInt( i, size )
  def readObject( i: Int ): Storable = 
    readObject( i + metadataSize, readSize( i ) ).asInstanceOf[ Storable ]
  def writeObjectBytes( i: Int, bytes: Array[ Byte ] ) =
    writeBytes( i + 5, bytes )

  // Convenience functions for reading an writing copied object metadata
  def setForwardingAddress( i: Int, a: Int ) = writeInt( i, a )
  def readForwardingAddress( i: Int ) = readInt( i )

  // Few objects grow by few bytes on serialization. This is a Java serialization bug.
  // Therefore, you need to add padding for these mutable objects.
  private def getPadding(s: Storable): Int = s match {
    case m: MayMutate => PADDING
    case _ => 0
  }
  
  //Garbage collect the heap
  private def doGC() {
    // FILL ME IN
    //
    // HINTS: here are the steps:
    //
    // 1. recursively traverse the live objects and copy them into 'to
    //    space' (this is handled by 'traceCopy', which assumes that
    //    the bumpPointer has already been pointed to 'to space');
    //    remember to update the addresses that point to the object when you
    //    copy them (this is why traceCopy returns the new address location)
    //
    // 2. swap 'from space' and 'to space'
    //
    // for updating, note carefully the definition of Address in
    // values.scala partcularly, that the 'loc' field is
    // mutable. this means that to update the reference you can just
    // assign the new value into that field, you don't need to create
    // a new Address at all
    //
    // for tracing, you can get the entire root-set by traversing
    // 'gEnv', the global environment. one important gotcha to be
    // careful of: the same Addresses can show up multiple times in
    // 'gEnv', meaning that you may come across a address when
    // traversing 'gEnv' that you've already updated (so don't trace
    // it, because it will already point into 'to space')
	
	bumpPointer = toStart
	trace("****************")
	trace("in do GC this is gEnv " + gEnv + " this is gEnv.env " + gEnv.env)
	trace("In doGC this is the gStore " +gStore.refMap)
	var sEnv = gEnv.env.toSet //get id of multiples
	//var lEnv = sEnv.toList
	sEnv.foreach( i => i match{
							case ( s: String, n: Int ) => { 
								trace("Getting address " + gStore.refMap.get(n))
								gStore.refMap.get(n) match {
									case Some(a:Address) => { val newAdd = traceCopy(a) //update refMap with new address? how?
																a.loc = newAdd}
									case _ => ()
									}
								}
						 case _ => ()
						})
						
	//call traceCopy and update address
	//swap spaces here ?? Is this done correctly??
	trace("In after doGC() what is in gEnv " + gEnv)
	trace("In after doGC this is the gStore " +gStore.refMap)
	val temp = fromStart
	fromStart = toStart
	toStart = temp
	trace("This is toStart " + toStart)
	trace("Setting bumpPointer " + bumpPointer)
	//bumpPointer = fromStart //fix this (could be mid)
  }
  
  // recursively copies from the given address
  // returns the address where the object is now
  // assumes that the fromStart, toStart, and bumpPointer
  // are all set properly
  def traceCopy( a: Address ): Int = {
    // FILL ME IN
    //
    // HINTS: here are the steps:
    //
    // 1. check if the object has already been copied; if so return
    //    the follow pointer, otherwise:
    //
    // 2. read the object from the heap
    //
    // 3. mark the object as copied and write the follow pointer into
    //    its old address (where the follow pointer will be the
    //    object's new address in 'to space')
    //
    // 4. allocate space for the object in 'to space' (but don't write
    //    the object there yet)
    //
    // 5. recursively trace the object's pointer addresses, updating them as
    //    necessary (this is why you didn't write the object to its
    //    new address yet); again, you can update the addresses using
    //    assignment, don't create a new object
    //
    // 6. now write the updated object to its address in 'to space'
    //    and return its new address location
	
	trace("In traceCopy is copied? " + isCopied(a.loc))
	if(isCopied(a.loc)) {
		readForwardingAddress( bumpPointer )
	}
	else {
		val o = readObject(a.loc)
		setCopied(a.loc)
		val asBytes = toBytes(o)
		val objSize = asBytes.size + getPadding(o)
		val fullSize =  metadataSize + objSize
		trace("TraceCopy bumpPointer " + bumpPointer)
		writeBytes( bumpPointer + 5, asBytes) //allocate space for object
		val postBump = bumpPointer
		bumpPointer = bumpPointer + fullSize
		trace("In traceCopy past writeBytes")
		//trace("In traceCopy object is " + o)
		o match{
			case c :CloV => { trace("TraceCopy CloV this is the Env " + c.env)
							var sEnv = c.env.env.toSet //get id of multiples
							//var lEnv = sEnv.toList
							sEnv.foreach( i => i match{
													case ( s: String, n: Int ) => { 
														trace("Getting address " + gStore.refMap.get(n))
														gStore.refMap.get(n) match {
															case Some(a:Address) => { //val newAdd = traceCopy(a) //update refMap with new address? how?
																						//a.loc = newAdd
																						trace("CloV's address is " + a)	
																						}
															case _ => ()
															}
														}
												 case _ => ()
												})



			} //Call trace copy on Env? 
			case l :ListCons => { trace("TraceCopy ListCons")
									l.value match{
										case ( sr: String, so: Storable ) => {
																			so match {
																				case a :Address => { trace("TraceCopy Address")
																									val newAdd = traceCopy(Address(a.loc))
																									a.loc = newAdd
																									 }
																				case _ => trace("TraceCopy object ListCons doesn't point to an address")
																			}}
										case _ => ()
									}
									val newAdd = traceCopy(l.next)  //what do we do with returned address
									l.next.loc = newAdd
									 }
			case ob :ObjectV => { trace("TraceCopy ObjectV")
									val newAdd = traceCopy(ob.head)
									ob.head.loc = newAdd
									 }
			case a :Address => { trace("TraceCopy Address")
									val newAdd = traceCopy(Address(a.loc))
									a.loc = newAdd
									 }
			case _ => trace("TraceCopy object doesn't point to an address")
		}
		trace("TraceCopy postBump " + postBump)
		setForwardingAddress( postBump, a.loc )
		writeInt( postBump, fullSize) 
		val nAddr = postBump
		trace("End of TraceCopy")
		nAddr
	}	
	
  }
  
  
  //---------- PUBLIC METHODS (FROM COLLECTOR TRAIT) ----------
  def gcAlloc( s: Storable ): Address = {
    trace( "## gcAlloc: allocating space for " + s )
    
    var asBytes = toBytes( s )
    val objSize = asBytes.size + getPadding(s)
    val fullSize =  metadataSize + objSize
	
    // FILL ME IN
    //
    // HINTS: here are the steps:
    //
    // 1. see if there's enough memory in 'from space' (remember to
    //    include room for the meta-data)
    // 2. if there is not enough memory:
    //    2a. collect the heap
    //    2b. see if there's enough memory now
    //    2c. if not, throw OOM exception
    // 3. write the object to memory (including meta-data)
    // 4. return the address of the object

    val postBump = bumpPointer + fullSize
	trace("Checking memory " + scala.math.abs(toStart-fromStart) + " need " + fullSize)
	if ( fromStart == 0 ) {
		if ( postBump >= ( heapSize / 2 ) ){
			doGC()
			//collect heap -> doGC()
			val postBump = bumpPointer + fullSize
			trace("Checking memory " + scala.math.abs(toStart-fromStart) + " need " + fullSize)
			if( fromStart == 0) {
				if(postBump > ( heapSize / 2 )){
					throw OOM
				}
			}
			else {
				if(postBump > ( heapSize )){
					throw OOM
				}
			}
		}
	}
	else {
		if ( postBump >= ( heapSize ) ){
			doGC()
			//collect heap -> doGC()
			val postBump = bumpPointer + fullSize
			trace("Checking memory " + scala.math.abs(toStart-fromStart) + " need " + fullSize)
			if( fromStart == 0) {
				if(postBump > ( heapSize / 2 )){
					throw OOM
				}
			}
			else {
				if(postBump > ( heapSize )){
					throw OOM
				}
			}
		}
	}
	  val index = bumpPointer //this is the index
	  bumpPointer = postBump
	
	trace("This is storable " + s)
	  s match {
      case m: MayMutate => {
        m.whereAllocated = index
        val newAsBytes = toBytes( m )
        assert( asBytes.length == newAsBytes.length )
        asBytes = newAsBytes ++ (0 until 50).map(_.toByte)
      }
      case _ => ()
    }	

	writeInt( index, fullSize)
    writeBytes( index + 5, asBytes)
	//writeObjectBytes(index, asBytes)
	
		trace( "## gcAlloc: allocated " + (asBytes.size + metadataSize) + 
           "bytes starting at addr " + index )
	
		trace("Returning addr " + Address(index) + " to Env")
	Address(index)
	//gStore.alloc(s)
  } // gcAlloc
  
  // GC Read function
  def gcRead( a: Address ): Storable = {
    trace( "## gcRead: reading " + a )
    assert(validAddress( a )) 
	//trace("a.loc is " + a.loc)
    val retval = readObject( a.loc )
    trace( "## gcRead: result == " + retval )
    retval
  }
  
  // Called for mutated objects
  def mutated( m: MayMutate ) {
    if ( m.whereAllocated != -1 ) {
      val asBytes = toBytes( m )
      trace( "## mutated: Flushing mutated object at " + m.whereAllocated )
      assert( readSize( m.whereAllocated ) >= asBytes.length)
      writeObjectBytes( m.whereAllocated, asBytes )
    }
  }

  // Update the addresses once changed
  def updateAddress( aOld: Address, aNew: Address ) {
    // overwrite the old address with the new one
    assert( aNew.whereAllocated == -1 )
    if ( aOld.whereAllocated != -1 ) {
      aNew.whereAllocated = aOld.whereAllocated
      val asBytes = toBytes( aNew )
      assert( readSize( aOld.loc ) == asBytes.length )
      writeObjectBytes( aOld.whereAllocated, asBytes )
    }
    aOld.loc = aNew.loc
  }

}