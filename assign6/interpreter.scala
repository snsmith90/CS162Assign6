import scala.io._

import scala.collection.mutable.{ Map => MMap }
import cs162.miniJS.syntax._
import cs162.miniJS.domains._
import cs162.miniJS.values._
import cs162.miniJS.gc._
import Value._
import Domains._

// the main entry point
object miniJS {
  // invoke with the following command:
  // scala miniJS -gc (stub|semi) -heap <heap size> [-trace] <program>
  //
  // <heap size> is in bytes. '-trace' is optional; if present it
  // turns on debugging messages. the command-line options must be in
  // the specified order.
  //


  def main(args:Array[String]) {
    
    val heapSize:Int = args(3).toInt 
    val gc = args(1) match {
      case "semi" => new SemispaceCollector(heapSize)
      case "stub" => new StubCollector(heapSize)
      case _ => throw undefined
    }
    
    val ast = if (args(4) == "-trace") {
      gc.tracep = true
      ParseL.getAST( Source.fromFile( args(5) ).mkString )
    }
    else {
      ParseL.getAST( Source.fromFile( args(4) ).mkString )
    }
    
    gStore.initStore(heapSize, gc)

    Interpreter eval ast
  }
  
}

// every term is evaluated inside a scope corresponding to a
// particular environment
object Interpreter {
  // gets the environment to use for the given function
  def closureEnv( fun: Fun ) =
    pruneEnv( fv( fun ) )

  // create an environment containing only variables in the given expression
  def pruneEnv( vars: Set[ Var ] ) = {
    val retval = Env()
    vars.foreach( v => 
      gEnv.env.find( _._1 == v.x ) match {
        case Some( b ) => retval.push( b )
        case _ => ()
      } )
    retval
  }

  // get the free variables in the given expression
  def fv( t: Term ): Set[ Var ] = {
    def fvSeq( es: Seq[ Term ] ) =
      es.foldLeft( Set[ Var ]() )( _ ++ fv( _ ) )

    t match {
      case _: Num | _: Bool | _: Str | _: Undef | _: In => Set()
      case v: Var => Set( v )
      case Fun( xs, t ) => fv( t ) -- xs.toSet
      case Let( xs, t ) => fv( t ) -- xs.toSet
      case Then( ts ) => fvSeq( ts )
      case Assign( x, e ) => fv( e ) + x
      case While( e, t ) => fv( e ) ++ fv( t )
      case Output( e ) => fv( e )
      case Update( e1, e2, e3 ) => fv( e1 ) ++ fv( e2 ) ++ fv( e3 )
      case UnOp( _, e ) => fv( e )
      case BinOp( _, e1, e2 ) => fv( e1 ) ++ fv( e2 )
      case If( e, t1, t2 ) => fv( e ) ++ fv( t1 ) ++ fv( t2 )
      case Call( ef, es ) => fv( ef ) ++ fvSeq( es )
      case MCall( e1, e2, es ) => fv( e1 ) ++ fv( e2 ) ++ fvSeq( es )
      case Obj( fbs ) => fvSeq( fbs.map( _._2 ) )
      case Access( e1, e2 ) => fv( e1 ) ++ fv( e2 )
    }
  }
    
  // evaluate a sequence of expressions returning a list of the
  // corresponding values
  def evalS( ts:Seq[Term] ): Seq[Storable] = 
    ts map( eval )

    // the main evaluation function
  def eval( t:Term ): Storable = t match {
    case Then( ts ) ⇒ 
      evalS( ts ).last
    
    // notice that e is evaluated _before_ we check whether x is legal
    case Assign( Var(x), e ) ⇒ 
    {
      val v = eval( e )
      gStore( gEnv( x ) ) = v
    }
    
    case w @ While( e, t ) ⇒ 
    {
      val v = eval( e )

      if ( v.T ) {
        eval( t )
        eval( w )
      }
      else UndefV()
    }

    case Output( e ) ⇒ 
    {
      val v = eval( e )

      println( v )
      UndefV()
    }
    
    case Update( e1, e2, e3 ) ⇒
    {
      val adr = eval( e1 )
      val fld = eval( e2 )
      val rhs = eval( e3 )

      (adr, fld) match {
        case (adr:Address, fld:StrV) ⇒ 
        {
          insertToObj(adr, fld, rhs)
          UndefV()
        }
        case _ ⇒ throw undefined
      }
    }

    case Num( n ) ⇒ 
      n

    case Bool( b ) ⇒ 
      b
      
    case Str( s ) ⇒ 
      s
      
    case Undef() ⇒ 
      UndefV()

    case Var( x ) ⇒ 
      gStore( gEnv( x ) )

    case UnOp( op, e ) ⇒ 
    {
      val v = eval( e )

      op match {
        case ⌞−⌟ ⇒ v neg
        case ⌞¬⌟ ⇒ v not
      }
    }
    
    case BinOp( op, e1, e2 ) ⇒ 
    {
      val v1 = eval( e1 )
      val v2 = eval( e2 )

      op match {
        case ⌜+⌝ ⇒ v1 + v2
        case ⌜−⌝ ⇒ v1 − v2
        case ⌜×⌝ ⇒ v1 × v2
        case ⌜÷⌝ ⇒ v1 ÷ v2
        case ⌜∧⌝ ⇒ v1 ∧ v2
        case ⌜∨⌝ ⇒ v1 ∨ v2
        case ⌜=⌝ ⇒ v1 ≈ v2
        case ⌜≠⌝ ⇒ v1 ≠ v2
        case ⌜≤⌝ ⇒ v1 ≤ v2
        case ⌜<⌝ ⇒ v1 < v2
      }
    }
    
    case If( e, t1, t2 ) ⇒ 
    {
      val v = eval( e )
      if ( v.T ) eval( t1 ) else eval( t2 )
    }

    case In( typ ) ⇒ 
      typ match {
        case NumIT ⇒ BigInt( Console.readLine() )
        case StrIT ⇒ Console.readLine()
      }
    
    case Let( xs, t ) ⇒ 
    {
      val bindings = xs map ( _.x → ( gStore += UndefV() ) )
      gEnv ++ bindings
      eval(t)
    }

    case fun@Fun( xs, t ) ⇒ 
      CloV( xs map (_.x), t, closureEnv(fun) )
    
    case Call( ef, es ) ⇒ 
    {
      val fun = eval( ef )
      fun match {
        case CloV(x_, _, _env) => gEnv ++ _env.env.map((mapping) => (FreshVar.getNext(), mapping._2))
        case _ => throw undefined
      }
      val args = gStore(gEnv( "gObj" )) +: evalS( es )
      applyClo( fun, args )
    }

    case MCall( e1, e2, es ) ⇒
    {
      val adr = eval( e1 )
      val fld = eval( e2 )

      val method = (adr, fld) match {
        case (adr:Address, fld:StrV) ⇒ toObj( gStore.read(adr) )( fld )
        case _ ⇒ throw undefined
      }
      method match {
        case CloV(x_, _, _env) => gEnv ++ _env.env.map((mapping) => (FreshVar.getNext(), mapping._2))
        case _ => throw undefined
      }

      val args = evalS( es )
      applyClo( method, adr +: args )
    }

    case Obj( fbs ) ⇒
    {
      val (xs, es) = fbs unzip; // semicolon required

      // fields and values
      val fs = xs map (_.s)
      val vs = evalS( es )

      val obj = createNewObj(fs, vs)

      val objAddr = gStore.alloc(obj)
      objAddr
    }

    case Access( e1, e2 ) ⇒
    {
      val adr = eval( e1 )
      val fld = eval( e2 )

      (adr, fld) match {
        case (adr:Address, fld:StrV) ⇒ toObj( gStore.read(adr) )( fld )
        case _ ⇒ throw undefined
      }
    }
  }

  // apply a closure
  def applyClo( fun:Storable, args:Seq[Storable] ): Storable =
    fun match { 
      case CloV(xs, t, _env) ⇒ 
      {
        if (xs.length != args.length) throw undefined
        else {
          _env.env.foreach( _ => gEnv.pop() ) //Remove the temporary bindings
          //Again add temporary bindings
          val bindings = (xs zip args) map ((pair) => {
            val ref = (gStore += pair._2)
            gEnv.push(pair._1 ->  ref)
            (pair._1, ref)
          })
          gEnv ++ _env.env //Add the closure environment
          val retval = eval( t )
          bindings.foreach( _ => gEnv.pop() )
          _env.env.foreach( _ => gEnv.pop() )
          retval
        }
      }
      case _ ⇒ throw undefined
    }

}
