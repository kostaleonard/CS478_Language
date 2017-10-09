package lang

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

//Not yet implemented:
//Break and Continue
//Classes (including Array functions)

/**
 * @author x74617
 */
class Interpreter {
  type Env = Map[String, Location]
  
  def interpret(program: List[Statement]): List[Value] = {
    val output = new ListBuffer[Value]()
    var returnVal = None: Option[Value]
    
    def arrayMult(arr: Arr, n: Int): Arr = {
      //Copies this array n times.
      //Returns a new Arr.
      if(n < 0) throw new InvalidExpressionException("Cannot multiply Array by negative integer.")
      var result = ArrayBuffer.empty[Value]
      for(num <- 0 until n) result ++= arr.buf
      Arr(result)
    }
    
    def eval(expr: Expr, env: Env): Value = expr match {
      case Ent(v) => Ent(v)
      case Flt(v) => Flt(v)
      case Str(s) => Str(s)
      case Chr(c) => Chr(c)
      case Bool(b) => Bool(b)
      case Null => Null
      case ArrExpr(list) =>
        val buf = list.map(eval(_, env)).to[ArrayBuffer]
        Arr(buf)
      case ArrAccess(arr, index) =>
        val ax = eval(arr, env)
        val ix = eval(index, env)
        (ax, ix) match{
          case (Arr(buf), Ent(index)) =>
            //Access the array.
            buf(index)
          case _ => throw new InvalidExpressionException("Unsupported operation: " + ax + "[" + ix + "]")
        }        
      case ArrSlice(arr, start, end) =>
        val ax = eval(arr, env)
        if(!ax.isInstanceOf[Arr]) throw new InvalidExpressionException("Unsupported operation: slice on " + ax)
        val sx = if(start == None) Ent(0) else eval(start.get, env)
        val ex = if(end == None) Ent(ax.asInstanceOf[Arr].buf.length) else eval(end.get, env)
        (ax, sx, ex) match{
          case (Arr(buf), Ent(s), Ent(e)) =>
            //A slice of the array has the same type as the array.
            Arr(buf.slice(s, e))
          case _ => throw new TypeException("Unsupported operation: slice indices not integers.")
        }  
      case Ident(name) =>
        //Lookup name in env.
        //If no mapping exists, throw new exception.
        //If mapping exists but contents are None, return null.
        //println("ident: "+name)
        env.getOrElse(name, throw new UnboundVariableException("Variable " + name + " does not have a binding. " + env)).contents.getOrElse(Null)
      case Pow(base, exp) => Flt(scala.math.pow(valToFloat(eval(base, env)), valToFloat(eval(exp, env))).toFloat) //Always returns a Flt
      case Mult(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (Arr(b1), Ent(v)) => arrayMult(Arr(b1), v)
          case (Ent(v), Arr(b1)) => arrayMult(Arr(b1), v)
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (Ent(n1), Ent(n2)) => Ent(valToInt(ex) * valToInt(ey))
          case (Flt(n1), Ent(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          case (Ent(n1), Flt(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          case (Flt(n1), Flt(n2)) => Flt(valToFloat(ex) * valToFloat(ey))
          //String operations
          case (Str(n1), Ent(v)) => Str(n1 * v)
          case (Ent(v), Str(n1)) => Str(n1 * v)
          //Unsupported operation:
          case _ => throw new InvalidExpressionException("Unsupported operation: " + ex + " * " + ey + ".")
        }
      case Div(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        if(ex.isInstanceOf[Ent] && ey.isInstanceOf[Ent]) Ent(valToInt(ex) / valToInt(ey))
        else Flt(valToFloat(ex) / valToFloat(ey))
      case DivTrunc(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        //For ints, this is identical to /
        //For floats, returns the floor of the division.
        if(ex.isInstanceOf[Ent] && ey.isInstanceOf[Ent]) Ent(valToInt(ex) / valToInt(ey))
        else Flt(math.floor(valToFloat(ex) / valToFloat(ey)).toFloat)
      case Mod(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        if(ex.isInstanceOf[Ent] && ey.isInstanceOf[Ent]) Ent(valToInt(ex) % valToInt(ey))
        else Flt(valToFloat(ex) % valToFloat(ey))
      case Add(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (Arr(b1), Arr(b2)) => Arr(b1 ++ b2) //join
          case (Arr(b1), v) => Arr(b1 :+ v) //append
          case (v, Arr(b1)) => Arr(v +: b1) //prepend
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (Ent(n1), Ent(n2)) => Ent(valToInt(ex) + valToInt(ey))
          case (Flt(n1), Ent(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          case (Ent(n1), Flt(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          case (Flt(n1), Flt(n2)) => Flt(valToFloat(ex) + valToFloat(ey))
          //If one of the two is a string, then this is a concatenation operation.
          case (Str(n1), other) => Str(n1 + other.toString)
          case (other, Str(n1)) => Str(other.toString + n1)
          //Unsupported operation:
          case _ => throw new InvalidExpressionException("Unsupported operation: " + ex + " + " + ey + ".")
        }
      case Sub(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        if(ex.isInstanceOf[Ent] && ey.isInstanceOf[Ent]) Ent(valToInt(ex) - valToInt(ey))
        else Flt(valToFloat(ex) - valToFloat(ey))
      case Not(e) => Bool(!valToBool(eval(e, env)))
      case IsEqual(x, y) => Bool(eval(x, env) == eval(y, env))
      case NotEqual(x, y) => Bool(eval(x, env) != eval(y, env))
      case LessThanEqual(x, y) => Bool(valToFloat(eval(x, env)) <= valToFloat(eval(y, env)))
      case GreaterThanEqual(x, y) => Bool(valToFloat(eval(x, env)) >= valToFloat(eval(y, env)))
      case LessThan(x, y) => Bool(valToFloat(eval(x, env)) < valToFloat(eval(y, env)))
      case GreaterThan(x, y) => Bool(valToFloat(eval(x, env)) > valToFloat(eval(y, env)))
      case And(x, y) => Bool(valToBool(eval(x, env)) && valToBool(eval(y, env)))
      case Or(x, y) => Bool(valToBool(eval(x, env)) || valToBool(eval(y, env)))
      case FunctCall(child, ident, args) =>
        //child is used in OOP.
        //Not yet implemented.
        val prev = env.get(ident.name)
        prev match{
          case Some(loc) =>
            //Make sure loc is storing a function.
            loc.contents match{
              case Some(FunctVal(t, a, body, staticEnv)) =>
                //Check the number and types of args
                //Type checking not yet implemented.
                if(args.params.length != a.params.length) throw new InvalidStatementException("Expected " + a.params.length + " args, but got " + args.params.length)
                //Perform actual function call.
                execFunct(FunctVal(t, a, body, staticEnv), args, env)
              case _ => throw new InvalidStatementException(loc.contents + " is not a function.")
            }
          case None => 
            //No previous mapping.
            //Throw exception.
            throw new UnboundVariableException("Function " + ident.name + " does not have a binding.")
        }
      case Field(child, ident) =>
        //If the child is none, this is just a variable (Ident).
        //Otherwise, it is a true object or class field.
        //println("call to "+Field(child, ident))
        if(child == None){
          //env.foreach(pair => println(pair._1 + ": " + pair._2.contents))
          eval(ident, env) 
        }
        else{
          val e = eval(ident, env)
          //println(ident + " is "+e)
          //if(!e.isInstanceOf[StructVal]) throw new InvalidStatementException("Access operator must be used on a struct.")
          e match{
            case Arr(buf) =>
              //could be accessing length method.
              if(child.get == Field(None, Ident("length"))) Ent(buf.length)
              else throw new InvalidExpressionException("Invalid expression: " + expr)
            case StructVal(c, f) =>
              //println("evaling: "+child.get + " in " + f)
              //f.foreach(pair => println(pair._1 + ": " + pair._2.contents))
              eval(child.get, f)
            case _ => e
          }          
        }
      case _ => throw new InvalidExpressionException("Invalid expression: " + expr)
    }
    
    def valToBool(v: Value): Boolean = v match {
      //Could also add here that non-zero ints and floats map to true.
      case Bool(x) => x
      case _ => throw new ConversionException("Cannot convert " + v + " to Boolean")
    }
    
    def valToInt(v: Value): Int = v match {
      case Ent(x) => x
      case Flt(x) => x.toInt
      case _ => throw new ConversionException("Cannot convert " + v + " to Int")
    }
    
    def valToFloat(v: Value): Float = v match {
      case Ent(x) => x.toFloat
      case Flt(x) => x
      case _ => throw new ConversionException("Cannot convert " + v + " to Float")
    }
    
    def declare(dec: Dec, env: Env): Env = dec match{
      case DecVar(t, ident, value) =>
        if(value == None) env + (ident.name -> new Location(t, None))
        else{
          var v = eval(value.get, env)
          v match{
            case Arr(buf) => v = Arr(buf.clone)
            case StructVal(t, f) =>
              //recreate the fields and their mappings in a different place in memory.
              var fields = Map.empty[String, Location]
              f.keys.foreach{ k =>
                val storedLoc = f(k)
                fields += (k -> new Location(storedLoc.typ, storedLoc.contents))
              }
              v = StructVal(t, fields)
            case _ => null
          }
          env + (ident.name -> new Location(t, Some(v)))
        }
      case DecFunct(t, ident, args, body) =>
        val funct = FunctVal(t, args, body, env)
        val newEnv = env + (ident.name -> new Location(t, Some(funct)))
        //place this function in its own environment to allow recursion.
        funct.staticEnv = newEnv
        //now add all previous function definitions in the block.
        addFunctionToBlock(ident.name, newEnv)
        newEnv
      case DecStruct(t, ident, fields) =>
        var fieldEnv = Map.empty[String, Location] //an empty environment
        //Add entries to the env.
        fields.foreach{ d =>
          val newEnv = declare(d, fieldEnv)
          d match{
            case DecVar(t, i, v) => fieldEnv += (i.name -> newEnv(i.name))
            case DecFunct(t, i, a, b) => throw new InvalidStatementException("Functions are not allowed in struct declarations.")
            case DecStruct(t, i, f) => fieldEnv += (i.name -> newEnv(i.name))
          }
        }        
        val struct = StructVal(t, fieldEnv)
        val loc = new Location(t, Some(struct))
        //Add this struct to its own environment
        struct.fieldEnv += (ident.name -> loc)
        env + (ident.name -> loc)
    }
    
    def addFunctionToBlock(functName: String, env: Env){
      //Adds the mapping of env(functName) to all functions in the env.
      //Used for mutual recursion.
      for((name, loc) <- env){
        loc.contents match{
          case Some(FunctVal(t, args, body, staticEnv)) => loc.contents.get.asInstanceOf[FunctVal].staticEnv += (functName -> env(functName))
          case _ => null
        }
      }
    }
    
    def reassign(re: Reassign, env: Env): Env = {
      val v = eval(re.expr, env)
      var modEnv = env
      var name = re.variable.ident.name
      re.variable.child match{
        case Some(Field(c, i)) =>
          val struct = eval(re.variable.ident, env).asInstanceOf[StructVal]
          modEnv = struct.fieldEnv
          name = i.name
        case None => null
        case _ => throw new InvalidStatementException("Invalid use of access operation.")
      }
      //println("Env: "+modEnv)
      val prev = modEnv.get(name)
      prev match{
        case Some(loc) =>
          //Previous mapping exists.
          loc.contents = Some(v)
          env
        case None => 
          //No previous mapping.
          //Throw exception.
          throw new UnboundVariableException("Variable " + name + " does not have a binding.")
      }
    }
    
    def arrayMod(am: ArrayMod, env: Env): Env = {
      val v = eval(am.newValue, env)
      val prev = env.getOrElse(am.arrVar.name, throw new UnboundVariableException("Variable " + am.arrVar.name + " does not have a binding."))
      val arr = prev.contents.get.asInstanceOf[Arr]
      //Reassignment of the buffer example:
      //arr.buf = List[Value](Ent(1)).to[ArrayBuffer]
      //arr.buf(0) = Ent(1)
      def arrayModHelp(arr: Arr, access: ArrAccess): Unit = {
        access.arr match{
          case ArrAccess(array, index) =>
            //Multidimensional array
            //println("sub: " + arr.buf(eval(access.index, env).asInstanceOf[Ent].value).asInstanceOf[Arr])
            arrayModHelp(arr.buf(eval(access.index, env).asInstanceOf[Ent].value).asInstanceOf[Arr], ArrAccess(array, index))
          case ArrSlice(array, start, end) => throw new InvalidStatementException("Cannot use array slice in array modification.")
          case _ =>
            //This is an actual array
            //val array = eval(access.arr, env).asInstanceOf[Arr]
            //array.buf(eval(access.index, env).asInstanceOf[Ent].value) = eval(am.newValue, env)
            arr.buf(eval(access.index, env).asInstanceOf[Ent].value) = eval(am.newValue, env)
        }
      }
      arrayModHelp(arr, am.access)      
      env
    }
    
    def exec(stmts: List[Statement], env: Env): Unit = {
      if(!returnVal.isEmpty) return
      stmts match {
      case Output(e) +: rest =>
        val result = eval(e, env)
        result match{
          case Arr(buf) => output += Arr(buf.clone()) 
          case _ => output += result
        }
        exec(rest, env)
      case BlockStatement(body) +: rest =>
        exec(body, env)
        exec(rest, env)
      case ArrayMod(ident, arrAccess, expr) +: rest =>
        arrayMod(ArrayMod(ident, arrAccess, expr), env)
        exec(rest, env)
      case If(cond, body, otherwise) +: rest =>
        if (valToBool(eval(cond, env))) exec(List(body), env)
        else for(stmt <- otherwise) exec(List(stmt), env)
        exec(rest, env)
      case While(cond, body) +: rest =>
        while(valToBool(eval(cond, env))) exec(List(body), env)
        exec(rest, env)
      case For(dec, cond, change, body) +: rest =>
        var newEnv = env
        if(dec != None) newEnv = declare(dec.get, env)
        while (valToBool(eval(cond, newEnv))) {
          exec(List(body), newEnv)
          exec(List(change), newEnv)
        }
        exec(rest, env)
      case Break +: rest => ???
      case Continue +: rest => ???
      case (e: Expr) +: rest =>
        eval(e, env)
        exec(rest, env)
      case DecVar(t, ident, value) +: rest =>
        exec(rest, declare(DecVar(t, ident, value), env))
      case DecFunct(t, ident, args, body) +: rest =>
        exec(rest, declare(DecFunct(t, ident, args, body), env))
      case DecStruct(t, ident, f) +: rest=>
        exec(rest, declare(DecStruct(t, ident, f), env))
      case Reassign(ident, value) +: rest =>
        exec(rest, reassign(Reassign(ident, value), env))
      case Return(None) +: rest => returnVal = Some(null)
      case Return(Some(expr)) +: rest => returnVal = Some(eval(expr, env))
      case _ => return //Empty list
      }
    }
    
    def execFunct(funct: FunctVal, args: Args, env: Env): Value = {
      //exec the body with the new env.                
      //Arguments have the name of the function's args.
      var newEnv = funct.staticEnv
      for(i <- 0 to funct.args.params.length -1){
      //for(i <- 0 until funct.args.params.length){
        val expr = args.params(i)
        //Add fields if struct
        /*
        val t = funct.args.params(i).t
        t match{
          case UserClassType(name, params) =>
            //Add all the fields to the environment
            val struct = env(name).asInstanceOf[StructVal]
            
            for(k <- env.keys.filter(_.startsWith(name + "."))){
              newEnv += (funct.args.params(i).ident.name + k.substring(name.length) -> env(k))
            }
          case _ => null          
        }       
        */
        val callByReference = funct.args.params(i).callByReference
        if(callByReference){
          expr match{
            case Ident(x) => 
              //Call-by-reference
              //Entry in newEnv points the function definition's arg name to the already stored location.
              newEnv += (funct.args.params(i).ident.name -> env(x))
            case Field(None, Ident(x)) =>
              //Call-by-reference
              //Note that OOP is not yet implemented.
              newEnv += (funct.args.params(i).ident.name -> env(x))
            case _ =>
              //Unsupported argument
              throw new InvalidExpressionException("Argument must be a variable to be compatible with call-by-reference function.")
          }
        }
        else{
          //Call-by-value
          var v = eval(expr, env)
          v match{
            case Arr(buf) => v = Arr(buf.clone)
            case StructVal(t, f) =>
              //println("structure argument: "+expr)
              //recreate the fields and their mappings in a different place in memory.
              var fields = Map.empty[String, Location]
              f.keys.foreach{ k =>
                val storedLoc = f(k)
                fields += (k -> new Location(storedLoc.typ, storedLoc.contents))
              }
              v = StructVal(t, fields)
              //fields.foreach(pair => println(pair._1 + ": " + pair._2.contents))
            case _ => null
          }
          newEnv += (funct.args.params(i).ident.name -> new Location(funct.args.params(i).t, Some(v))) 
        }          
      }
      
      exec(funct.body.stmts, newEnv)
      
      if(!returnVal.isEmpty){
        val ret = returnVal.get
        returnVal = None
        ret
      }
      else null
    }
    
    exec(program, Map.empty[String, Location])
    output.toList
  }
  
  def test(program: List[Statement], expected: List[Value]) {
    val actual = interpret(program)
    if (actual == expected) println("Passed.")
    else {
      println(s"""FAILED TEST
      program = $program
      expected $expected
      but got  $actual""")
    }
  }
  
  def testProgram(text: String){
    val tokens = new Scanner().scanner(text)
    val parsed = new Parser().parser(tokens)
    interpret(parsed).foreach(println)
    println("Passed:\n" + text)
  }
  
  def runTests(){
    //Output
    test(List(Output(Ent(5))), List(Ent(5)))
    //Float
    test(List(Output(Flt(5.0.toFloat))), List(Flt(5.0.toFloat)))
    //String
    test(List(Output(Str("hello world"))), List(Str("hello world")))
    //Char
    test(List(Output(Chr('!'))), List(Chr('!')))
    //Bool
    test(List(Output(Bool(true))), List(Bool(true)))
    //Pow
    test(List(Output(Pow(Ent(4), Ent(2)))), List(Flt(16.0.toFloat)))
    //test(Output(Pow(Str("hello"), Ent(2))), List()) //throws error correctly
    //Mult
    test(List(Output(Mult(Ent(4), Ent(2)))), List(Ent(8)))
    test(List(Output(Mult(Ent(-1), Ent(2)))), List(Ent(-2)))
    test(List(Output(Mult(Flt(4.0.toFloat), Ent(2)))), List(Flt(8.0.toFloat)))
    //Div
    test(List(Output(Div(Ent(4), Ent(2)))), List(Ent(2)))
    test(List(Output(Div(Flt(5.0.toFloat), Ent(2)))), List(Flt(2.5.toFloat)))
    test(List(Output(Div(Flt(4.0.toFloat), Ent(2)))), List(Flt(2.0.toFloat)))
    //DivTrunc
    test(List(Output(DivTrunc(Ent(4), Ent(2)))), List(Ent(2)))
    test(List(Output(DivTrunc(Flt(5.0.toFloat), Ent(2)))), List(Flt(2.0.toFloat)))
    test(List(Output(DivTrunc(Flt(4.0.toFloat), Ent(2)))), List(Flt(2.0.toFloat)))
    test(List(Output(DivTrunc(Flt(-10.3.toFloat), Ent(2)))), List(Flt(-6.0.toFloat)))
    //Mod
    test(List(Output(Mod(Ent(4), Ent(2)))), List(Ent(0)))
    test(List(Output(Mod(Ent(5), Ent(2)))), List(Ent(1)))
    test(List(Output(Mod(Flt(5.0.toFloat), Ent(2)))), List(Flt(1.0.toFloat)))
    test(List(Output(Mod(Flt(5.0.toFloat), Flt(2.6.toFloat)))), List(Flt(2.4.toFloat)))
    test(List(Output(Mod(Ent(-5), Ent(2)))), List(Ent(-1)))
    test(List(Output(Mod(Flt(-5.5.toFloat), Ent(2)))), List(Flt(-1.5.toFloat)))
    //Add
    test(List(Output(Add(Ent(4), Ent(2)))), List(Ent(6)))
    test(List(Output(Add(Ent(-1), Ent(2)))), List(Ent(1)))
    test(List(Output(Add(Flt(4.0.toFloat), Ent(2)))), List(Flt(6.0.toFloat)))
    //Sub
    test(List(Output(Sub(Ent(4), Ent(2)))), List(Ent(2)))
    test(List(Output(Sub(Ent(-1), Ent(2)))), List(Ent(-3)))
    test(List(Output(Sub(Flt(4.0.toFloat), Ent(2)))), List(Flt(2.0.toFloat)))
    //Boolean Ops
    test(List(Output(Not(Bool(true)))), List(Bool(false)))
    test(List(Output(Not(Bool(false)))), List(Bool(true)))
    test(List(Output(IsEqual(Bool(true), Bool(true)))), List(Bool(true)))
    test(List(Output(IsEqual(Ent(1), Ent(0)))), List(Bool(false)))
    test(List(Output(IsEqual(Str("hello"), Str("hello")))), List(Bool(true)))
    test(List(Output(IsEqual(Flt(-1.0.toFloat), Ent(1)))), List(Bool(false)))
    test(List(Output(NotEqual(Bool(true), Bool(true)))), List(Bool(false)))
    test(List(Output(NotEqual(Ent(1), Ent(0)))), List(Bool(true)))
    test(List(Output(NotEqual(Str("hello"), Str("hello")))), List(Bool(false)))
    test(List(Output(NotEqual(Flt(-1.0.toFloat), Ent(1)))), List(Bool(true)))
    test(List(Output(LessThanEqual(Ent(1), Ent(0)))), List(Bool(false)))
    test(List(Output(LessThanEqual(Ent(1), Ent(1)))), List(Bool(true)))
    test(List(Output(LessThanEqual(Ent(1), Ent(6)))), List(Bool(true)))
    test(List(Output(LessThan(Ent(1), Ent(0)))), List(Bool(false)))
    test(List(Output(LessThan(Ent(1), Ent(1)))), List(Bool(false)))
    test(List(Output(LessThan(Ent(1), Ent(6)))), List(Bool(true)))
    test(List(Output(GreaterThanEqual(Ent(1), Ent(0)))), List(Bool(true)))
    test(List(Output(GreaterThanEqual(Ent(1), Ent(1)))), List(Bool(true)))
    test(List(Output(GreaterThanEqual(Ent(1), Ent(6)))), List(Bool(false)))
    test(List(Output(GreaterThan(Ent(1), Ent(0)))), List(Bool(true)))
    test(List(Output(GreaterThan(Ent(1), Ent(1)))), List(Bool(false)))
    test(List(Output(GreaterThan(Ent(1), Ent(6)))), List(Bool(false)))
    test(List(Output(And(Bool(true), Bool(true)))), List(Bool(true)))
    test(List(Output(And(Bool(false), Bool(true)))), List(Bool(false)))
    test(List(Output(And(Bool(true), Bool(false)))), List(Bool(false)))
    test(List(Output(And(Bool(false), Bool(false)))), List(Bool(false)))
    test(List(Output(Or(Bool(true), Bool(true)))), List(Bool(true)))
    test(List(Output(Or(Bool(false), Bool(true)))), List(Bool(true)))
    test(List(Output(Or(Bool(true), Bool(false)))), List(Bool(true)))
    test(List(Output(Or(Bool(false), Bool(false)))), List(Bool(false)))
    
    //Variables and Scope
    test(List(DecVar(IntType, Ident("x"), Some(Ent(5))), Output(Ident("x"))), List(Ent(5)))
    test(List(DecVar(IntType, Ident("x"), None), Output(Ident("x"))), List(null))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(5))), //int x = 5;
              DecVar(IntType, Ident("x"), Some(Ent(7))), //int x = 7;
              Output(Ident("x"))),
              List(Ent(7)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(true), //if true:
                  DecVar(IntType, Ident("x"), Some(Ent(1))), //int x = 1;
                  None), //(no else)
              Output(Ident("x"))),
              List(Ent(0)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(true), //if true:
                  BlockStatement(List(DecVar(IntType, Ident("x"), Some(Ent(1))), //int x = 1;
                                      Output(Ident("x")))),
                  None), //(no else)
              Output(Ident("x"))),
              List(Ent(1), Ent(0)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(true), //if true:
                  Output(Ident("x")),
                  None), //(no else)
              Output(Ident("x"))),
              List(Ent(0), Ent(0)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(true), //if true:
                  BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(1)), //x = 1;
                                      Output(Ident("x")))),
                  None), //(no else)
              Output(Ident("x"))),
              List(Ent(1), Ent(1)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(true), //if true:
                  BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(1)), //x = 1;
                                      Output(Ident("x")))), //print(x);
                  Some(If(Bool(true), //else:
                           BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(2)), //x = 2;
                                          Output(Ident("x")))), //print(x);
                           None))), //(no else)
              Output(Ident("x"))),
              List(Ent(1), Ent(1)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(false), //if false:
                  BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(1)), //x = 1;
                                      Output(Ident("x")))), //print(x);
                  Some(If(Bool(true), //else:
                           BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(2)), //x = 2;
                                          Output(Ident("x")))), //print(x);
                           None))), //(no else)
              Output(Ident("x"))),
              List(Ent(2), Ent(2)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              If(Bool(false), //if false:
                  BlockStatement(List(Reassign(Field(None, Ident("x")), Ent(1)), //x = 1;
                                      Output(Ident("x")))), //print(x);
                  Some(If(Bool(true), //else:
                           BlockStatement(List(DecVar(IntType, Ident("x"), Some(Ent(2))), //int x = 2;
                                          Output(Ident("x")))), //print(x);
                           None))), //(no else)
              Output(Ident("x"))),
              List(Ent(2), Ent(0)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              While(LessThan(Ident("x"), Ent(5)), //while x < 5:
                  BlockStatement(List(Output(Ident("x")), //print(x);
                                      Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1)))))), //x = x + 1;
              Output(Ident("x"))), //print(x);
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4), Ent(5)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              While(GreaterThan(Ident("x"), Ent(5)), //while x > 5:
                  BlockStatement(List(Output(Ident("x")), //print(x);
                                      Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1)))))), //x = x + 1;
              Output(Ident("x"))), //print(x);
              List(Ent(0)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              While(LessThan(Ident("x"), Ent(5)), //while x < 5:
                  BlockStatement(List(Output(Ident("x")), //print(x);
                                      Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1))), //x = x + 1;
                                      DecVar(IntType, Ident("x"), Some(Ent(0)))))), //int x = 0;
              Output(Ident("x"))), //print(x);
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4), Ent(5)))
    //Throws error correctly:
    /*
    test(List(For(Some(DecVar(IntType, Ident("x"), Some(Ent(0)))), //for int x = 0; x < 5; x++:
                  LessThan(Ident("x"), Ent(5)),
                  Reassign(Ident("x"), Add(Ident("x"), Ent(1))),
                      BlockStatement(List(Output(Ident("x"))))), //print(x);
              Output(Ident("x"))), //print(x);
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4), Ent(4)))     
    */
    test(List(For(Some(DecVar(IntType, Ident("x"), Some(Ent(0)))), //for int x = 0; x < 5; x++:
                  LessThan(Ident("x"), Ent(5)),
                  Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1))),
                      BlockStatement(List(Output(Ident("x")))))), //print(x);
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              For(None, //for x < 5; x++:
                  LessThan(Ident("x"), Ent(5)),
                  Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1))),
                      BlockStatement(List(Output(Ident("x"))))), //print(x);
              Output(Ident("x"))),
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4), Ent(5)))    
    //Functions
    test(List(DecFunct(FunctType(List.empty, NullType), Ident("f"), TypedArgs(List.empty), BlockStatement(List(Output(Ent(1))))), //void f(){ return 1; };
              FunctCall(None, Ident("f"), Args(List.empty))), //f();
              List(Ent(1)))
    test(List(DecFunct(FunctType(List.empty, IntType), Ident("f"), TypedArgs(List.empty), BlockStatement(
                       List(Return(Some(Ent(1)))))), //void f(){ return 1; };
              DecVar(IntType, Ident("x"), Some(FunctCall(None, Ident("f"), Args(List.empty)))), //int x = f();
              Output(Ident("x"))), //print(x);
              List(Ent(1)))
    //Call-by-value Parameters
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              DecFunct(FunctType(List(IntType), NullType), Ident("f"), TypedArgs(List(Arg(IntType, false , Ident("y")))), BlockStatement(
                       List(Output(Ident("y"))))), //void f(int y){ print(y); };
              For(None, //for x < 5; x++:
                  LessThan(Ident("x"), Ent(5)),
                  Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1))),
                      BlockStatement(List(FunctCall(None, Ident("f"), Args(List(Ident("x"))))))), //f(x);
              Output(Ident("x"))),
              List(Ent(0), Ent(1), Ent(2), Ent(3), Ent(4), Ent(5)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              DecFunct(FunctType(List(IntType), NullType), Ident("f"), TypedArgs(List(Arg(IntType, false , Ident("x")))), BlockStatement(
                       List(Output(Ident("x"))))), //void f(int x){ print(x); };
              For(None, //for x < 5; x++:
                  LessThan(Ident("x"), Ent(5)),
                  Reassign(Field(None, Ident("x")), Add(Ident("x"), Ent(1))),
                      BlockStatement(List(FunctCall(None, Ident("f"), Args(List(Add(Ident("x"), Ent(1)))))))), //f(x+1);
              Output(Ident("x"))),
              List(Ent(1), Ent(2), Ent(3), Ent(4), Ent(5), Ent(5))) 
    //Call-by-reference
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              DecFunct(FunctType(List(IntType), NullType), Ident("f"), TypedArgs(List(Arg(IntType, true , Ident("y")))), BlockStatement(
                       List(Reassign(Field(None, Ident("y")), Add(Ident("y"), Ent(1)))))), //void f(~int y){ y=y+1; };
              FunctCall(None, Ident("f"), Args(List(Ident("x")))), //f(x);
              Output(Ident("x"))),
              List(Ent(1)))
    //Recursion
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              //void incTo3(~int y){
              DecFunct(FunctType(List(IntType), NullType), Ident("incTo3"), TypedArgs(List(Arg(IntType, true , Ident("y")))), BlockStatement(List(
                  If(IsEqual(Ident("y"), Ent(3)), Return(None), None), //if(y == 3) return;
                  Reassign(Field(None, Ident("y")), Add(Ident("y"), Ent(1))), //y++;
                  FunctCall(None, Ident("incTo3"), Args(List(Ident("y"))))))), //incTo3(y);
              FunctCall(None, Ident("incTo3"), Args(List(Ident("x")))), //f(x);
              Output(Ident("x"))),
              List(Ent(3)))
    test(List(DecVar(IntType, Ident("x"), Some(Ent(0))), //int x = 0;
              //void incTo3(~int y){
              DecFunct(FunctType(List(IntType), NullType), Ident("incTo3"), TypedArgs(List(Arg(IntType, true , Ident("y")))), BlockStatement(List(
                  If(IsEqual(Ident("y"), Ent(3)), Return(None), None), //if(y == 3) return;
                  Reassign(Field(None, Ident("y")), Add(Ident("y"), Ent(1))), //y++;
                  FunctCall(None, Ident("incTo3"), Args(List(Ident("y"))))))), //incTo3(y);
              Output(Ident("x"))),
              List(Ent(0)))
    test(List(
              //void factorial(int y){
              DecFunct(FunctType(List(IntType), NullType), Ident("factorial"), TypedArgs(List(Arg(IntType, false , Ident("y")))), BlockStatement(List(
                  If(IsEqual(Ident("y"), Ent(0)), Return(Some(Ent(1))), None), //if(y == 0) return1;
                  Return(Some(Mult(Ident("y"), FunctCall(None, Ident("factorial"), Args(List(Sub(Ident("y"), Ent(1))))))))))), //return y*factorial(y-1);
              Output(FunctCall(None, Ident("factorial"), Args(List(Ent(5)))))), //print(factorial(5));
              List(Ent(120)))
    //Static vs Dynamic Scoping
    /*
    void outer() {
      int x = 1;
  
      void inner1() {
        print(x);
      }
  
      void inner2() {
        int x = 2;
        inner1();
      }
  
      inner2();
    }
    outer(); //Should be 1
    */
    test(List(DecFunct(FunctType(List.empty, NullType), Ident("outer"), TypedArgs(List.empty), BlockStatement(List(
                  DecVar(IntType, Ident("x"), Some(Ent(1))), //int x = 1;
                  DecFunct(FunctType(List.empty, NullType), Ident("inner1"), TypedArgs(List.empty), BlockStatement(List(
                    Output(Ident("x"))    
                  ))), 
                  DecFunct(FunctType(List.empty, NullType), Ident("inner2"), TypedArgs(List.empty), BlockStatement(List(
                    DecVar(IntType, Ident("x"), Some(Ent(2))),
                    FunctCall(None, Ident("inner1"), Args(List.empty))
                  ))),
                  FunctCall(None, Ident("inner2"), Args(List.empty))
                  ))),
              FunctCall(None, Ident("outer"), Args(List.empty))),
              List(Ent(1)))       
    //Mutual Recursion
    /*
    bool isEven(int n){
      if n == 0 {return true};
      return isOdd(n - 1);
    }
    bool isOdd(int n){
      if n == 0 {return false};
      return isEven(n - 1);
    }    
    */
    test(List(DecFunct(FunctType(List(IntType), BoolType), Ident("isEven"), TypedArgs(List(Arg(IntType, false , Ident("n")))), BlockStatement(List(
                    If(IsEqual(Ident("n"), Ent(0)), Return(Some(Bool(true))), None),
                    Return(Some(FunctCall(None, Ident("isOdd"), Args(List(Sub(Ident("n"), Ent(1)))))))
                  ))), 
                  DecFunct(FunctType(List(IntType), BoolType), Ident("isOdd"), TypedArgs(List(Arg(IntType, false , Ident("n")))), BlockStatement(List(
                    If(IsEqual(Ident("n"), Ent(0)), Return(Some(Bool(false))), None),
                    Return(Some(FunctCall(None, Ident("isEven"), Args(List(Sub(Ident("n"), Ent(1)))))))                    
                  ))),
              Output(FunctCall(None, Ident("isEven"), Args(List(Ent(0))))),
              Output(FunctCall(None, Ident("isEven"), Args(List(Ent(1))))),
              Output(FunctCall(None, Ident("isEven"), Args(List(Ent(2))))),
              Output(FunctCall(None, Ident("isOdd"), Args(List(Ent(101)))))
              ),
              List(Bool(true), Bool(false), Bool(true), Bool(true))) 
     /*
    int fib2(int n){ return fib(n-2); }
    int fib1(int n){ return fib(n-1); }
    int fib(int n){
      if(n > 1) return fib1(n) + fib2(n);
      else return 1;
    }
    */
    test(List(    DecFunct(FunctType(List(IntType), IntType), Ident("fib2"), TypedArgs(List(Arg(IntType, false , Ident("n")))), BlockStatement(List(
                    Return(Some(FunctCall(None, Ident("fib"), Args(List(Sub(Ident("n"), Ent(2)))))))
                  ))),
                  DecFunct(FunctType(List(IntType), IntType), Ident("fib1"), TypedArgs(List(Arg(IntType, false , Ident("n")))), BlockStatement(List(
                    Return(Some(FunctCall(None, Ident("fib"), Args(List(Sub(Ident("n"), Ent(1)))))))
                  ))), 
                  DecFunct(FunctType(List(IntType), IntType), Ident("fib"), TypedArgs(List(Arg(IntType, false , Ident("n")))), BlockStatement(List(
                    If(GreaterThan(Ident("n"), Ent(1)), Return(Some(Add(
                        FunctCall(None, Ident("fib1"), Args(List(Ident("n")))),
                        FunctCall(None, Ident("fib2"), Args(List(Ident("n"))))))),
                        Some(If(Bool(true), Return(Some(Ent(1))), None))
                        )                 
                  ))),
              Output(FunctCall(None, Ident("fib"), Args(List(Ent(1))))),
              Output(FunctCall(None, Ident("fib"), Args(List(Ent(2))))),
              Output(FunctCall(None, Ident("fib"), Args(List(Ent(5))))),
              Output(FunctCall(None, Ident("fib"), Args(List(Ent(10)))))
              ),
              List(Ent(1), Ent(2), Ent(8), Ent(89))) 
    //Arrays
    test(List(DecVar(IntType, Ident("arr"), Some(ArrExpr(List.empty))), //int arr = [];
              Output(Ident("arr"))), //print(arr);
              List(Arr(ArrayBuffer.empty)))
    test(List(DecVar(IntType, Ident("arr"), Some(ArrExpr(List(Ent(1))))), //int arr = [1];
              Output(Ident("arr"))), //print(arr);
              List(Arr(ArrayBuffer(Ent(1))))) 
    test(List(DecVar(IntType, Ident("arr"), Some(ArrExpr(List(Ent(1), Flt(1.0.toFloat), Ent(3))))), //int arr = [1, 1.0, 3];
              Output(Ident("arr"))), //print(arr);
              List(Arr(ArrayBuffer(Ent(1), Flt(1.0.toFloat), Ent(3)))))
    test(List(DecVar(IntType, Ident("arr"), Some(ArrExpr(List(Ent(1))))), //int arr = [1];
              Reassign(Field(None, Ident("arr")), Add(Ident("arr"), Ent(2))), //arr += 2;
              Output(Ident("arr"))), //print(arr);
              List(Arr(ArrayBuffer(Ent(1), Ent(2))))) 
    test(List(DecVar(IntType, Ident("arr"), Some(ArrExpr(List(Ent(1))))), //int arr = [1];
              Reassign(Field(None, Ident("arr")), Mult(Ident("arr"), Ent(3))), //arr *= 3;
              Output(Ident("arr"))), //print(arr);
              List(Arr(ArrayBuffer(Ent(1), Ent(1), Ent(1))))) 
    testProgram("Array<int> arr = [1, 2, 3, 4];")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[0] = 9; print(arr);")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[3] = 9; print(arr);")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[0] = 9; print(arr[0]);")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[0] = 9; print(arr[1:2]);")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[0] = 9; print(arr[:]);")
    testProgram("Array<int> arr = [1, 2, 3, 4]; arr[0] = 9; print(arr[:2]);")
    testProgram("Array<Array<int>> arr = [[1, 2, 3, 4], [1]]; arr[0] = [9]; print(arr);")
    testProgram("Array<Array<int>> arr = [[1, 2, 3, 4], [1]]; arr[0] = []; arr[0] = [5]; print(arr);")
    testProgram("Array<Array<int>> arr = [[1, 2, 3, 4], [1]]; arr[0][0] = 9; print(arr);")
    testProgram("Array<Array<Array<int>>> arr = [[[1, 2, 3, 4], [1]], [[]]]; arr[0][0][0] = 9; print(arr);")
    testProgram("struct node{ int x = 0; } print(node.x);")
    testProgram("struct node{ int x = 0; } node n = node; n.x = 3; print(node.x); print(n.x);")
    testProgram("struct node{ int x = 0; } node n = node; n.x = 3; node.x = 5; print(node.x); print(n.x);")
    testProgram("struct node{ int x = 0; } node n1 = node; n1.x = 3; node.x = 5; node n2 = node; print(n1.x); print(n2.x);")
    testProgram("struct node{ node next; int value = 0; } node n = node; n.next = node;")
    testProgram("struct node{ node next; int value = 0; } node n = node; n.next = node; n.next.value = 2; print(n.next.value);")
    testProgram("struct node{ node next; int value = 0; } node n = node; n.next = node; n.next.next = node; n.next.next.value = 3; print(n.next.next.value);")
    testProgram("struct node<A> { A value; }")
  }
}