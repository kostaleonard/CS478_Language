package lang

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

/**
 * @author x74617
 */
class Typechecker {
  type Env = Map[String, Type]
  
  def typecheck(program: List[Statement]): Unit = {
    var returnVals = List.empty[Option[Type]]
    
    def eval(expr: Expr, env: Env): Type = expr match {
      case Ent(v) => IntType
      case Flt(v) => FltType
      case Str(s) => StrType
      case Chr(c) => ChrType
      case Bool(b) => BoolType
      case Null => NullType
      case ArrExpr(list) =>
        //Check to make sure that all types in the array match.
        //Note that this does not account for types with a common ancestor class.
        //Could use try-catch wherever this array was expected (i.e. argument, declaration).
        //If the array throws a TypeException when evaled, double check the common super class that relates the two
        //(all types are subtypes of Object).
        //Check this versus the expected Array type; throw Exception if not matching.
        if(list.isEmpty) ArrayType(EmptyArrayType)
        else{
          val list2 = list.map(e => eval(e, env))
          val type1 = list2.head
          if(list2.filterNot{t =>
            t match{
              case `type1` => true
              case ArrayType(param) => type1.isInstanceOf[ArrayType] && compatibleArrayTypes(ArrayType(param), type1.asInstanceOf[ArrayType])
              case _ => false
            }}.nonEmpty) throw new TypeException("Arrays must have elements of common type.")
          ArrayType(type1)
        }
      case ArrAccess(arr, index) =>
        val ax = eval(arr, env)
        val ix = eval(index, env)
        (ax, ix) match{
          case (ArrayType(param), IntType) =>
            //An element of the array has the same type as the type param.
            ax.asInstanceOf[ArrayType].typeParam
          case (ArrayType(param), other) => throw new TypeException("Array access parameter must be of type int.")
          case (other1, other2) => throw new TypeException("Cannot perform array access operation on type " + other1)
        }        
      case ArrSlice(arr, start, end) =>
        val ax = eval(arr, env)
        val sx = if(start == None) IntType else eval(start.get, env)
        val ex = if(end == None) IntType else eval(end.get, env)
        (ax, sx, ex) match{
          case (ArrayType(param), IntType, IntType) =>
            //A slice of the array has the same type as the array.
            ax
          case (ArrayType(param), other1, other2) => throw new TypeException("Array slice parameters must be of type int.")
          case (other1, other2, other3) => throw new TypeException("Cannot perform array slice operation on type " + other1)
        }  
      case Ident(name) =>
        //Lookup name in env.
        //If no mapping exists, throw new exception.
        //If mapping exists but contents are None, return null.
        env.getOrElse(name, throw new UnboundVariableException("Variable " + name + " does not have a binding."))
      case Pow(base, exp) =>
        val (b, e) = (eval(base, env), eval(exp, env))
        (b, e) match{
          case (IntType, IntType) => FltType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          case _ => throw new TypeException("Unsupported types for ** operator: " + b + ", " + e)
        }
      case Mult(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (ArrayType(param), IntType) => ArrayType(param)
          case (IntType, ArrayType(param)) => ArrayType(param)
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //String operations
          case (StrType, IntType) => StrType
          case (IntType, StrType) => StrType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for * operator: " + ex + ", " + ey)
        }
      case Div(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for / operator: " + ex + ", " + ey)
        }
      case DivTrunc(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        //For ints, this is identical to /
        //For floats, returns the floor of the division.
        (ex, ey) match{
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for // operator: " + ex + ", " + ey)
        }
      case Mod(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for % operator: " + ex + ", " + ey)
        }
      case Add(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        (ex, ey) match{
          case (ArrayType(param1), ArrayType(param2)) =>
            if(param1 == param2) ArrayType(param1)
            else throw new TypeException("Could not combine Arrays of types: " + param1 + ", " + param2)
          case (ArrayType(param1), otherType) =>
            if(param1 == otherType) ArrayType(param1)
            else throw new TypeException("Could not append element of type " + otherType + " to Array of type " + param1)
          case (otherType, ArrayType(param1)) =>
            if(param1 == otherType) ArrayType(param1)
            else throw new TypeException("Could not append element of type " + otherType + " to Array of type " + param1)
          //If both are Ents, keep as Ent. Otherwise convert to Flt.
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //If one of the two is a string, then this is a concatenation operation.
          //The other value is converted to a string with toString.
          case (StrType, other) => StrType
          case (other, StrType) => StrType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for + operator: " + ex + ", " + ey)
        }
      case Sub(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (IntType, IntType) => IntType
          case (IntType, FltType) => FltType
          case (FltType, IntType) => FltType
          case (FltType, FltType) => FltType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for - operator: " + ex + ", " + ey)
        }
      case Not(e) =>
        val t = eval(e, env)
        if(t != BoolType) throw new TypeException("Expected boolean, but found " + t)
        else BoolType
      case IsEqual(x, y) =>
        //Any two objects (of any types) can be compared without error.
        BoolType
      case NotEqual(x, y) =>
        //Any two objects (of any types) can be compared without error.
        BoolType
      case LessThanEqual(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (IntType, IntType) => BoolType
          case (IntType, FltType) => BoolType
          case (FltType, IntType) => BoolType
          case (FltType, FltType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for <= operator: " + ex + ", " + ey)
        }
      case GreaterThanEqual(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (IntType, IntType) => BoolType
          case (IntType, FltType) => BoolType
          case (FltType, IntType) => BoolType
          case (FltType, FltType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for >= operator: " + ex + ", " + ey)
        }        
      case LessThan(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (IntType, IntType) => BoolType
          case (IntType, FltType) => BoolType
          case (FltType, IntType) => BoolType
          case (FltType, FltType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for < operator: " + ex + ", " + ey)
        }
      case GreaterThan(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (IntType, IntType) => BoolType
          case (IntType, FltType) => BoolType
          case (FltType, IntType) => BoolType
          case (FltType, FltType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for > operator: " + ex + ", " + ey)
        }
      case And(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (BoolType, BoolType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for and operator: " + ex + ", " + ey)
        }
      case Or(x, y) =>
        val ex = eval(x, env)
        val ey = eval(y, env)
        //If both are Ents, keep as Ent. Otherwise convert to Flt.
        (ex, ey) match{
          case (BoolType, BoolType) => BoolType
          //Unsupported operation:
          case _ => throw new TypeException("Unsupported types for <= operator: " + ex + ", " + ey)
        }
      case FunctCall(child, ident, args) =>
        //Assuming the function itself has been typechecked properly,
        //it should return an object of its return type.
        val functType = env.getOrElse(ident.name, throw new UnboundVariableException("Variable " + ident.name + " does not have a binding."))
        functType match{
          case FunctType(inParams, returnType) =>
            //Typecheck args
            if(args.params.length != inParams.length) throw new InvalidStatementException("Expected " + inParams.length + " args, but got " + args.params.length)
            for(i <- 0 until args.params.length){
              val e = args.params(i)
              val t = eval(e, env)
              if(t != inParams(i) && t != NullType) throw new TypeException("Expected " + inParams(i) + ", but got " + t)
            }          
            //Args were the correct type
            returnType
          case other =>
            //This is not a function
            throw new TypeException(ident.name + " is not a Function.")
        }
      case Field(child, ident) =>
        //If the child is none, this is just a variable (Ident).
        //Otherwise, it is a true struct or class field.
        //if(child == None) eval(ident, env)
        //else throw new InvalidExpressionException("Invalid expression: " + expr)
        var name = ident.name
        var c = child
        while(c.getOrElse(None).isInstanceOf[Field]){
          val field = c.get.asInstanceOf[Field]
          name += "." + field.ident.name
          c = field.child
        }
        if(name.endsWith(".length")){
          val arr = eval(Ident(name.substring(0, name.length-7)), env)   
          if(!arr.isInstanceOf[ArrayType]) throw new TypeException("length operator must be called on an Array.")
          IntType
        }        
        else eval(Ident(name), env)        
      case _ => throw new InvalidExpressionException("Invalid expression: " + expr)
    }
    
    def declare(dec: Dec, env: Env): Env = dec match{
      case DecVar(t, ident, value) =>
        if(value != None){
          //Check type
          val v = eval(value.get, env)
          v match{
            case `t` => null
            case ArrayType(EmptyArrayType) => if(!t.isInstanceOf[ArrayType]) throw new TypeException("Function value does not match return type. Expected: " + t + ", Found: " + ArrayType)
            case other => throw new TypeException("Variable " + ident.name + ": expected " + t + ", found " + other)
          }
        }
        var fieldEnv = Map.empty[String, Type]
        t match{
          case UserClassType(name, params) =>
            //Also have to add the types of the fields in the user class
            for(k <- env.keys.filter(_.startsWith(name + "."))){
              fieldEnv += (ident.name + k.substring(name.length) -> env(k))
            }
          case _ => null
        }
        env ++ fieldEnv + (ident.name -> t)
      case DecFunct(t, ident, args, body) =>
        //Typecheck the function when it is declared.
        //Artificially add locations to the staticEnv (they will be thrown out anyway in eval, but it is still necessary for recursion).
        val funct = FunctVal(t, args, body, env.mapValues( t => new Location(t, None)))
        //place this function in its own environment to allow recursion.
        funct.staticEnv = funct.staticEnv + (ident.name -> new Location(t, Some(funct)))
        val newEnv = funct.staticEnv.mapValues(loc => loc.typ)
        //Typecheck the function
        execFunct(funct, args, newEnv)
        //Return the updated environment
        newEnv
      case DecStruct(t, ident, fields) =>
        //typecheck the fields
        var fieldEnv = Map.empty[String, Type] //an empty environment
        //Add entries to the env.
        fields.foreach{ d =>
          val newEnv = declare(d, fieldEnv)
          d match{
            case DecVar(t, i, v) => fieldEnv += (ident.name + "." + i.name -> newEnv(i.name)) //node.next points to type node
            case DecFunct(t, i, a, b) => throw new InvalidStatementException("Functions are not allowed in struct declarations.")
            case DecStruct(t, i, f) => throw new NotImplementedException("Nested structs not yet implemented.")//fieldEnv += (ident.name + "." + i.name -> newEnv(i.name))
          }
        }  
        //The fields are added to the environment using dot notation.
        //Note that these can never be accidentally called, as dots are not allowed in identifiers.
        //Instead, when a struct object calls on one of its fields, the typechecker can access it via dot notation.
        //println("fields: " + fieldEnv)
        //println("env: " + (env ++ fieldEnv + (ident.name -> t)))
        env ++ fieldEnv + (ident.name -> t)
    }
    
    def compatibleArrayTypes(prev: ArrayType, next: ArrayType): Boolean = (prev, next) match{
      //Returns true if the ArrayTypes are compatible.
      case (ArrayType(ArrayType(param1)), ArrayType(ArrayType(param2))) => compatibleArrayTypes(ArrayType(param1), ArrayType(param2))
      case (ArrayType(param), ArrayType(EmptyArrayType)) => true
      case (ArrayType(EmptyArrayType), ArrayType(param)) => true
      case _ => false
    }
    
    def reassign(re: Reassign, env: Env): Env = {
      val newType = eval(re.expr, env)
      var name = re.variable.ident.name
      var variable = re.variable
      var flag = true
      while(variable.child.nonEmpty && flag){
        variable.child.get match{
          case Field(None, i) =>
            name += "." + i.name
            flag = false
          case Field(Some(c), i) =>
            name += "." + i.name
            variable = Field(Some(c), i)
          case _ => throw new InvalidStatementException("Invalid use of access operation.")
        }
      }
      //val prevType = env.getOrElse(re.variable.name, throw new UnboundVariableException("Variable " + re.variable.name + " does not have a binding."))
      val prevType = env.getOrElse(name, throw new UnboundVariableException("Variable " + name + " does not have a binding."))
      if(prevType.isInstanceOf[ArrayType] && newType.isInstanceOf[ArrayType] && compatibleArrayTypes(prevType.asInstanceOf[ArrayType], newType.asInstanceOf[ArrayType]))
        //It is OK to reassign an instantiated array to an empty array.
        return env
      if(newType != prevType) throw new TypeException(name + ": cannot reassign " + prevType + " to " + newType)
      env
    }
    
    def exec(stmts: List[Statement], env: Env): Unit = {
      //Typecheck a List of Statements.
      //if(!returnVal.isEmpty) return
      stmts match {
      case Output(e) +: rest =>
        exec(rest, env)
      case ArrayMod(ident, arrAccess, expr) +: rest =>
        val arrType = eval(ident, env)
        arrType match{
          case ArrayType(param) => null
          case other => throw new TypeException("Unsupported operation for type " + other)
        }
        val param = eval(arrAccess, env)
        //val param = arrType.asInstanceOf[ArrayType].typeParam
        eval(expr, env) match{
          case `param` => null
          case ArrayType(EmptyArrayType) => null
          case other => throw new TypeException("Cannot convert " + other + " to type " + param)       
        }
        exec(rest, env)
      case BlockStatement(body) +: rest =>
        exec(body, env)
        exec(rest, env)
      case If(cond, body, otherwise) +: rest =>
        eval(cond, env) match{
          case BoolType => null //this is correct; do nothing
          case other => throw new TypeException("If condition requires boolean. Found: " + other)
        }
        exec(List(body), env)
        for(stmt <- otherwise) exec(List(stmt), env)
        exec(rest, env)
      case While(cond, body) +: rest =>
        eval(cond, env) match{
          case BoolType => null //this is correct; do nothing
          case other => throw new TypeException("While condition requires boolean. Found: " + other)
        }
        exec(List(body), env)
        exec(rest, env)
      case For(dec, cond, change, body) +: rest =>
        var newEnv = env
        if(dec != None) newEnv = declare(dec.get, env)
        eval(cond, newEnv) match{
          case BoolType => null //this is correct; do nothing
          case other => throw new TypeException("For condition requires boolean. Found: " + other)
        }
        exec(List(body), newEnv)
        exec(List(change), newEnv)
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
      case Return(None) +: rest => returnVals :+= None
      case Return(Some(expr)) +: rest => returnVals :+= Some(eval(expr, env))
      case _ => return //Empty list
      }
    }
    
    def execFunct(funct: FunctVal, args: TypedArgs, env: Env): Unit = {
      //exec the body with the new env.                
      //Arguments have the name of the function's args.
      var newEnv = funct.staticEnv.mapValues( loc => loc.typ )
      for(i <- 0 to funct.args.params.length - 1){
        val t = funct.args.params(i).t
        t match{
          case UserClassType(name, params) =>
            //Add all the fields to the environment
            for(k <- env.keys.filter(_.startsWith(name + "."))){
              newEnv += (funct.args.params(i).ident.name + k.substring(name.length) -> env(k))
            }
          case _ => null          
        }
        newEnv += (funct.args.params(i).ident.name -> t) 
      }
      
      //Bad implementation of mutual recursion:
      //newEnv = addFunctions(env, newEnv)
      
      //Execute the function and check that the return type matched what was received.
      try{
        exec(funct.body.stmts, newEnv)
      }
      catch{
        case UnboundVariableException(msg) =>
          //This occurs when the TypeChecker encounters a situation of mutual recursion.
          //Because the function is typechecked immediately, it will not recognize the second function.
          //Therefore, this error is ignored and caught at runtime (if it is actually an error).
          null
      }
      
      //Check to make sure that all returned types match:
      while(returnVals.nonEmpty){
        val returnVal = returnVals.head
        var ret: Type = NullType
        returnVal match{
          case Some(t) => ret = t
          case None => ret = NullType
        }
        ret match{
          case funct.t.returnType =>
            //Return type matches returned value
            ;
          case ArrayType(EmptyArrayType) =>
            if(!funct.t.returnType.isInstanceOf[ArrayType]) throw new TypeException("Function value does not match return type. Expected: " + funct.t.returnType + ", Found: " + ArrayType)
          case other =>
            //Return type does not match
            throw new TypeException("Function value does not match return type. Expected: " + funct.t.returnType + ", Found: " + other)
        }
        returnVals = returnVals.tail
      }
    }
    
    exec(program, Map.empty[String, Type])
  }
  
  def test(text: String){
    val tokens = new Scanner().scanner(text)
    val parsed = new Parser().parser(tokens)
    typecheck(parsed)
    println("Passed:\n" + text)
  }
  
  def runTests(){
    println("Starting tests.")
    var program = "int x;"
    test(program)
    program = "int x = 0;"
    test(program)
    //Throws exception correctly:
    program = "int x = 'a';"
    //test(program)
    //Throws exception correctly:
    program = "int x = [];"
    //test(program)
    program = "float x = 0.0;"
    test(program)
    program = "String x = \"hello\";"
    test(program)
    program = "char x = 'a';"
    test(program)
    program = "bool x = false;"
    test(program)
    program = "bool x = true;"
    test(program)
    program = "int x = 10 + 20;"
    test(program)
    program = "bool x = true or false == 5;"
    test(program)
    program = "float x = 3**4;"
    test(program)
    program = "int x = 10; int y = x % 2;"
    test(program)
    program = "int one(){ return 1; } int x = one() + 2;"
    test(program)
    program = "int one(){ return 1; } float x = one() + 2.0;"
    test(program)
    //Throws error correctly:
    program = "int one(){ return 1; } float x = one() + 2;"
    //test(program)
    program = "int x = 5 * 3;"
    test(program)
    program = "int x = 5 / 3;"
    test(program)
    program = "int x = 9 // 2;"
    test(program)
    program = "int x = 1 - 2;"
    test(program)
    //Throws error correctly:
    program = "int x = 0 == 1;"
    //test(program)
    program = "bool x = true == true;"
    test(program)
    program = "bool x = true != false or (5-2) == 3;"
    test(program)
    program = "bool x = 2.0 <= 5;"
    test(program)
    program = "bool x = 0 < 1 or 1.0 > 2 or 1 >= 5.0;"
    test(program)
    program = "int x = 0; x = 5;"
    test(program)
    //Throws error correctly:
    program = "int x = 0; x = 'a';"
    //test(program)
    program = "int x = 1; if x == 0 { print(1); }"
    test(program)
    //Throws error correctly:
    program = "int x = 0; if x-2 {}"
    //test(program)
    //Throws error correctly:
    program = "{bool x = 0;}"
    //test(program)
    program = "for int x = 0, x < 5, x++ { int q = 2; }"
    test(program)
    //Throws error correctly:
    program = "for int x = true, x < 5, x++ { int q = 2; }"
    //test(program)
    program = "for int x = 0, x + 5, x++ { int q = 2; }"
    //test(program)
    //Throws error correctly:
    program = "for int x = 0, x < 5, x + 1 { int q = 2; }"
    //test(program)
    //Throws error correctly:
    program = "for int x = 0, x < 5, x++ { bool q = 2; }"
    //test(program)
    program = "while true { int q = 2; }"
    test(program)
    //Throws error correctly:
    program = "while 1 + 2 { int q = 2; }"
    //test(program)
    //Throws error correctly:
    program = "while true { float q = 2; }"
    //test(program)
    program = "void doSomething(int x, char y){ x = x + 1; return; }"
    test(program)
    //Throws error correctly:
    program = "int doSomething(int x, char y){ x = x + 1; return; }"
    //test(program)
    //Throws error correctly:
    program = "void doSomething(int x, char y){ x = false; return; }"
    //test(program)
    program = "void doSomething(~int x, char y){ x = x + 1; return; }"
    test(program)
    program = "void doSomething(int x, char y){ x = x + 1; return; } doSomething(7, 'a');"
    test(program)
    //Throws error correctly:
    program = "void doSomething(int x, char y){ x = x + 1; return; } doSomething(0.0, 'a');"
    //test(program)
    program = "int fact(int n){ if n == 0 { return 1; } return n * fact(n - 1); }"
    test(program)
    //Throws error correctly:
    program = "int fact(int n){ if n == 0 { return 1; } bool x = 1; return n * fact(n - 1); }"
    //test(program)
    //Throws error correctly:
    program = "int fact(int n){ if n == 0 { return 1; } int x = 1; return n * fact(true); }"
    //test(program)
    //Throws error correctly:
    program = "bool fact(int n){ if n == 0 { return 1; } return n * fact(n - 1); }"
    //test(program)
    //Throws error correctly:
    program = "bool fact(int n){ if n == 0 { return 1; } return fact(n - 1); }"
    //test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; int x = arr[0];"
    test(program)
    //Throws exception correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; float x = arr[0];"
    //test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; int x = arr[1-1];"
    test(program)
    //Throws exception correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; int x = arr[0.0];"
    //test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; int index = 1; int x = arr[index];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; int foo(){ return 0; } int x = arr[foo()];"
    test(program)
    //Throws error correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; bool foo(){ return false; } int x = arr[foo()];"
    //test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; Array<int> sub = arr[0:1];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; Array<int> sub = arr[:1];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; Array<int> sub = arr[0:];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; Array<int> sub = arr[:];"
    test(program)
    //Throws error correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; Array<int> sub = arr[false:1];"
    //test(program)
    //Throws error correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; float sub = arr[0:1];"
    //test(program)
    program = "bool x = [false, false, true][1];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; arr[0] = 0;"
    test(program)
    //Throws error correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; arr[0] = 'a';"
    //test(program)
    //Throws error correctly:
    program = "Array<int> arr = [1, 2, 3, 4]; arr[false] = 0;"
    //test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; int x = arr[0][0];"
    test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; Array<int> sub = arr[0][1:2]; int x = sub[0];"
    test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; Array<int> sub = arr[0]; int x = sub[0];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; arr = [1];"
    test(program)
    program = "Array<int> arr = [1, 2, 3, 4]; arr = [];"
    test(program)
    program = "Array<int> arr = []; arr = [];"
    test(program)
    program = "Array<int> arr = []; arr = [1];"
    test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; arr = [[]];"
    test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; arr[0] = [1];"
    test(program)
    program = "Array<Array<int>> arr = [[1, 2, 3, 4], [5, 6, 7, 8]]; arr[0] = []; arr[0] = []; arr[0] = [1];"
    test(program)
    program = "Array<Array<Array<int>>> arr = [[[1, 2, 3, 4], [5, 6, 7, 8]], []]; int x = arr[0][0][0];"
    test(program)
    program = "Array<Array<Array<int>>> arr = [[[1, 2, 3, 4], [5, 6, 7, 8]], []]; arr[0][0][0] = 1;"
    test(program)
    //Unsupported operation:
    program = "Array<Array<Array<int>>> arr = [[[1, 2, 3, 4], [5, 6, 7, 8]], []]; arr[0][0][1:2] = [1];"
    //test(program)
    program = "Array<Array<Array<int>>> arr = [[[1, 2, 3, 4], [5, 6, 7, 8]], []]; arr[0][0:][1] = [1];"
    test(program)
    program = "struct node{ int value = 0; }"
    test(program)
    program = "struct node{ int value = 0; } print(node.value);"
    test(program)
    program = "struct node{ int value = 0; } int x = node.value;"
    test(program)
    //Throws error correctly:
    program = "struct node{ int value = 0; } bool x = node.value;"
    //test(program)
    program = "struct node{ int value = 0; } node.value = 10;"
    test(program)
    program = "struct node{ int value = 0; } node n = node; n.value = 99;"
    test(program)
    //Throws error correctly:
    program = "struct node{ int value = 0; } node n = node; n.value = false;"
    //test(program)
    println("Done.")
  }
}