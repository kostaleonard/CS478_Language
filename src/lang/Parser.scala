package lang

import scala.collection.mutable.ListBuffer

/**
 * @author x74617
 */
class Parser {
  def parser(tokens: List[Token]): List[Statement] = {
    def parseAll(toks: List[Token]): List[Statement] = {
      //Converts all tokens into statements.
      //Will not quit until all tokens are consumed.
      val statements = new ListBuffer[Statement]()
      var remaining = toks
      while(!remaining.isEmpty){
        val (nextStatement, rem) = parseStatement(remaining)
        statements += nextStatement
        remaining = rem
      }
      statements.toList
    }
    
    def semicolonChecker(toks: List[Token]): List[Token] = toks match{
      //Check for a semicolon token at the end of a statement.
      //Return the tokens after the semicolon.
      case SemiColon +: rest => rest //this is correct; do nothing.
      case _ => throw new StatementParseException("; expected.")
    }
    
    def parseArrayMod(array: Ident, toks: List[Token]): (ArrayMod, List[Token]) = {
      //Returns an ArrayMod and the list of unconsumed tokens.
      //Begins after the [.  
      val (arrAccess, rest) = parseArrayOps(array, toks)
      if(!arrAccess.isInstanceOf[ArrAccess]) throw new StatementParseException("Unsupported array operation.")
      rest match{
        case Equals +: rest2 =>
          val (e, rest3) = parseExpr(rest2)
          (ArrayMod(array, arrAccess.asInstanceOf[ArrAccess], e), rest3)
        case _ => throw new StatementParseException("= expected.")
      }      
    }
    
    def parseStatement(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Returns the statement that results from the input, and a List of the unconsumed Tokens.
      //If a statement begins with a token not described in the CFG, throws Exception.
      case LParen +: rest => 
        var (stmt: Statement, rest2) = parseExpr(LParen +: rest)
        rest2 match{
          case LBrack +: rest3 =>
            if(stmt.isInstanceOf[Ident]){
              val (stmt2, rest4) = parseArrayMod(stmt.asInstanceOf[Ident], rest3)
              stmt = stmt2
              rest2 = rest4}
            else throw new StatementParseException("; expected.")
        }
        (stmt, semicolonChecker(rest2))
      case LBrack +: rest => 
        val (stmt, rest2) = parseExpr(LBrack +: rest)
        (stmt, semicolonChecker(rest2))
      case LCurly +: rest => parseBlockStatements(rest)
      case Minus +: rest => //Could begin a negative number
        val (stmt, rest2) = parseExpr(Minus +: rest)
        (stmt, semicolonChecker(rest2))
      case Ent(v) +: rest => 
        val (stmt, rest2) = parseExpr(Ent(v) +: rest)
        (stmt, semicolonChecker(rest2))
      case Flt(v) +: rest => 
        val (stmt, rest2) = parseExpr(Flt(v) +: rest)
        (stmt, semicolonChecker(rest2))
      case Str(n) +: rest => 
        val (stmt, rest2) = parseExpr(Str(n) +: rest)
        (stmt, semicolonChecker(rest2))
      case Chr(n) +: rest => 
        val (stmt, rest2) = parseExpr(Chr(n) +: rest)
        (stmt, semicolonChecker(rest2))
      case Bool(v) +: rest =>
        val (stmt, rest2) = parseExpr(Bool(v) +: rest)
        (stmt, semicolonChecker(rest2))
      //Be careful with Symbol.
      //Could begin an expression, a declaration, or a reassignment statement.
      case Symbol(s1) +: Dot +: Symbol(s2) +: rest =>
        //try to parse reassignment, but if fail then parse an expr
        try{
          //println("parsing reassignment of a field: "+ s1 + "." + s2)
          val (stmt, rest2) = parseReassignment(toks)
          (stmt, semicolonChecker(rest2))
        }
        catch{
          case StatementParseException(msg) =>
            val (stmt, rest2) = parseExpr(toks)
            (stmt, semicolonChecker(rest2))
        }
      case Symbol(n) +: rest =>
        val (stmt, rest2) = parseSymbol(Symbol(n) +: rest)
        (stmt, semicolonChecker(rest2))
      case NotTok +: rest => 
        val (stmt, rest2) = parseExpr(NotTok +: rest)
        (stmt, semicolonChecker(rest2))
      case ForTok +: rest => parseFor(rest)
      case IfTok +: rest => 
        val (ifStatement, rest2) = parseIf(IfTok +: rest)
        (ifStatement, rest2)
      case WhileTok +: rest => parseWhile(rest)
      case ReturnTok +: rest => parseReturn(rest)
      case ContinueTok +: SemiColon +: rest => (Continue, rest)
      case BreakTok +: SemiColon +: rest => (Break, rest)     
      case PrintTok +: LParen +: rest =>
        val (expr, rest2) = parseExpr(LParen +: rest)
        (Output(expr), semicolonChecker(rest2))
      case StructTok +: rest => parseStructDec(rest)
      //Anything else is an invalid start of a statement.
      case _ => throw new StatementParseException("Invalid symbol: "+toks.head)
    }
    
    def parseReassignment(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses reassignment.
      case Symbol(s1) +: Dot +: Symbol(s2) +: rest =>
        //This is the reassignment of a field
        val (e, rest2) = parseExpr(toks)
        //println("parsed expr: "+e)
        //println("rest: "+rest2)
        if(!e.isInstanceOf[Field]) throw new StatementParseException("Invalid reassignment syntax; expected field.")
        val field = e.asInstanceOf[Field]
        rest2 match{
          case Equals +: rest3 =>
            val (body, rest4) = parseExpr(rest3)
            (Reassign(field, body), rest4)
          case _ => throw new StatementParseException("= expected.")
        }
      case Symbol(s1) +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Field(None, Ident(s1)), body), rest2)
      }      
      case Symbol(s1) +: Plus +: Plus +: rest => (Reassign(Field(None, Ident(s1)), Add(Ident(s1), Ent(1))), rest)
      case Symbol(s1) +: Minus +: Minus +: rest => (Reassign(Field(None, Ident(s1)), Sub(Ident(s1), Ent(1))), rest)
      case Symbol(s1) +: Plus +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Field(None, Ident(s1)), Add(Ident(s1), body)), rest2)
      }      
      case Symbol(s1) +: Minus +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Field(None, Ident(s1)), Sub(Ident(s1), body)), rest2)
      } 
      case Symbol(s1) +: Ast +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Field(None, Ident(s1)), Mult(Ident(s1), body)), rest2)
      } 
      case Symbol(s1) +: Slash +: Equals +: rest => {
        val (body, rest2) = parseExpr(rest)
        (Reassign(Field(None, Ident(s1)), Div(Ident(s1), body)), rest2)
      } 
      case _ => throw new StatementParseException("Invalid symbol: " + toks.head)
    }
    
    def parseSymbol(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses a Symbol.
      //toks begins with a Symbol.
      //A Symbol may begin an expression, a declaration, or a reassignment.
      case Symbol(s1) +: Symbol(s2) +: rest => parseDec(toks)
      case Symbol(s1) +: LBrack +: rest =>
        try{
          parseArrayMod(Ident(s1), rest)
        }
        catch{
          case StatementParseException(msg) => parseArrayOps(Ident(s1), rest)
        }
      case Symbol(s1) +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Plus +: Plus +: rest => parseReassignment(toks)
      case Symbol(s1) +: Minus +: Minus +: rest => parseReassignment(toks)
      case Symbol(s1) +: Plus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Minus +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Ast +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: Slash +: Equals +: rest => parseReassignment(toks)
      case Symbol(s1) +: rest => 
        try{
          parseDec(toks)
        }
        catch{
          case MalformedDeclarationException(msg) => parseExpr(toks)
        }
      case _ => throw new StatementParseException("Invalid symbol.")
    }
    
    def parseFor(toks: List[Token]): (Statement, List[Token]) = {
      //Parses a For statement.
      //Begins after ForTok.
      var (dec, rest2) = (None: Option[DecVar], toks)
      try{
        val (dec0, rest0) = parseDec(toks)
        dec = Some(dec0.asInstanceOf[DecVar])
        rest2 = rest0
        if(rest2.headOption != Some(Comma)) throw new StatementParseException(", expected.")
        rest2 = rest2.tail
      }
      catch{
        case MalformedDeclarationException(msg) => null //there is no Dec at the beginning of the statement.
      }
      val (cond, rest3) = parseExpr(rest2)
      if(rest3.headOption != Some(Comma)) throw new StatementParseException(", expected.")
      val (reassign, rest4) = parseReassignment(rest3.tail)
      if(rest4.headOption != Some(LCurly)) throw new StatementParseException("{ expected.")
      val (body, rest5) = parseBlockStatements(rest4.tail)
      (For(dec, cond, reassign.asInstanceOf[Reassign], body), rest5)
    }
    
    def parseIf(toks: List[Token]): (If, List[Token]) = toks match{
      //Parses an If statement.
      //Begins with the IfTok.
      case IfTok +: rest => {
        val (cond, rest2) = parseExpr(rest)
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. { expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        if(rest3.headOption == Some(ElifTok) || rest3.headOption == Some(ElseTok)) {
          val (otherwise, rest4) = parseIf(rest3)
          (If(cond, body, Some(otherwise)), rest4)
        }
        else (If(cond, body, None), rest3)
      }
      case ElifTok +: rest => {
        val (cond, rest2) = parseExpr(rest)
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. { expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        if(rest3.headOption == Some(ElifTok) || rest3.headOption == Some(ElseTok)) {
          val (otherwise, rest4) = parseIf(rest3)
          (If(cond, body, Some(otherwise)), rest4)
        }
        else (If(cond, body, None), rest3)
      }
      case ElseTok +: rest => {
        if(rest.headOption != Some(LCurly)) throw new StatementParseException("Improper if syntax. { expected.")
        val (body, rest2) = parseBlockStatements(rest.tail)
        (If(Bool(true), body, None), rest2)
      }
      case _ => throw new StatementParseException("Invalid symbol.")
    }
    
    def parseWhile(toks: List[Token]): (Statement, List[Token]) = {
      //Parses a While statement.
      //Begins with the conditional.
      val (cond, rest) = parseExpr(toks)
      if(rest.headOption != Some(LCurly)) throw new StatementParseException("Improper while syntax. { expected.")
      val (stmt, rest2) = parseBlockStatements(rest.tail)
      (While(cond, stmt), rest2)
    }
    
    def parseReturn(toks: List[Token]): (Statement, List[Token]) = toks match{
      //Parses a Return statement.
      //Begins after the ReturnTok.
      case SemiColon +: rest => (Return(None), rest)
      case _ => {
        val (e, rest2) = parseExpr(toks)
        rest2 match{
          case SemiColon +: rest3 => (Return(Some(e)), rest3) //this is correct; do nothing.
          case _ => throw new StatementParseException("; expected.")
        }
      }
    }
        
    def parseExpr(toks: List[Token]): (Expr, List[Token]) = {
      //Parses an Expr from the front of toks and returns that Expr and the unconsumed Tokens.
      parseExpr7(toks)
    }
    
    def parseExprSymbol(toks: List[Token]): (Expr, List[Token]) = toks match{
      //Parses Symbols within Expressions.
      case Symbol(s1) +: LParen +: rest =>
        val (args, rest2) = parseArgs(rest)
        rest2 match{
          case Dot +: Symbol(s2) +: rest3 =>
            val (child, rest4) = parseExprSymbol(rest2.tail)
            (FunctCall(Some(child), Ident(s1), args), rest4)
          case LBrack +: rest3 => parseArrayOps(FunctCall(None, Ident(s1), args), rest3)
          case _ => (FunctCall(None, Ident(s1), args), rest2)
        }
      case Symbol(s1) +: Dot +: Symbol(s2) +: rest =>
        val (child, rest4) = parseExprSymbol(Symbol(s2) +: rest)
        //println("Field: " + Field(Some(child), Ident(s1)))
        (Field(Some(child), Ident(s1)), rest4)
      case Symbol(s1) +: LBrack +: rest => parseArrayOps(Field(None, Ident(s1)), rest)
      case Symbol(s1) +: rest => (Field(None, Ident(s1)), rest)
      /*
      case Symbol(s1) +: rest =>
        rest match{
          case Dot +: Symbol(s2) +: rest3 => 
            val (child, rest4) = parseExprSymbol(rest.tail)
            (Field(Some(child), Ident(s1)), rest4)
          case _ => (Field(None, Ident(s1)), rest)
        }
      */
      case _ => throw new StatementParseException("Invalid symbol: "+toks.head)
    }
    
    def parseArrayOps(array: Expr, toks: List[Token]): (Expr, List[Token]) = toks match{
      //Parses an Array operation (ArrAccess or ArrSlice)
      //Begins after [.
      //Access: int x = [0, 1, 2, 3][2]
      //Slice: Array<int> sub = [0, 1, 2, 3][1:2] or [1:] or [:2] or [:]
      case Colon +: RBrack +: LBrack +: rest => parseArrayOps(ArrSlice(array, None, None), rest)
      case Colon +: RBrack +: rest => (ArrSlice(array, None, None), rest)
      case Colon +: rest =>
        val (e1, rest2) = parseExpr(rest)
        rest2 match{
          case RBrack +: LBrack +: rest3 => parseArrayOps(ArrSlice(array, None, Some(e1)), rest3)
          case RBrack +: rest3 => (ArrSlice(array, None, Some(e1)), rest3)
          case _ => throw new StatementParseException("] expected.")
        }
      case rest =>
        val (e1, rest2) = parseExpr(rest)
        rest2 match{
          case Colon +: RBrack +: LBrack +: rest3 => parseArrayOps(ArrSlice(array, Some(e1), None), rest3)
          case Colon +: RBrack +: rest3 => (ArrSlice(array, Some(e1), None), rest3)
          case Colon +: rest3 =>
            val (e2, rest4) = parseExpr(rest3)
            rest4 match{
              case RBrack +: LBrack +: rest5 => parseArrayOps(ArrSlice(array, Some(e1), Some(e2)), rest5)
              case RBrack +: rest5 => (ArrSlice(array, Some(e1), Some(e2)), rest5)
              case _ => throw new StatementParseException("Unexpected symbol for array operation: " + rest4.head)
            }
          case RBrack +: LBrack +: rest3 => parseArrayOps(ArrAccess(array, e1), rest3)
          case RBrack +: rest3 => (ArrAccess(array, e1), rest3)
          case _ => throw new StatementParseException("Unexpected symbol for array operation: " + rest2.head)
        }
    }
    
    def parseArray(toks: List[Token]): (ArrExpr, List[Token]) = toks match{
      //Parses and returns an ArrExpr (implemented with a scala.collection.mutable.ArrayBuffer, the Scala version of Java's ArrayList).
      //Begins after first [.
      case RBrack +: rest => (ArrExpr(List.empty), rest)
      case _ => {
        val (arg, rest) = parseExpr(toks)
        rest match{
          case RBrack +: rest => (ArrExpr(List(arg)), rest)
          case Comma +: rest => {
            val (arr, rest2) = parseArray(rest)
            (ArrExpr(arg +: arr.contents), rest2)
          }
        }
      }
    }
    
    def parseExpr0(toks: List[Token]): (Expr, List[Token]) = toks match{
      //Atomic expressions.
      case Ent(v) +: rest => (Ent(v), rest)
      case Flt(v) +: rest => (Flt(v), rest)
      case Str(n) +: rest => (Str(n), rest)
      case Chr(n) +: rest => (Chr(n), rest)
      case Bool(v) +: rest => (Bool(v), rest)
      case Null +: rest => (Null, rest)
      case Minus +: Ent(v) +: rest => (Ent(-1*v), rest)
      case Minus +: Flt(v) +: rest => (Flt(-1*v), rest)
      //Symbol will be fully implemented when we begin doing more object-oriented programming.
      case Symbol(n) +: rest => parseExprSymbol(toks)
      case LParen +: rest => { 
        val (e, rest2) = parseExpr(rest)
        rest2 match{
          case RParen +: LBrack +: rest3 => parseArrayOps(e, rest3)
          case RParen +: rest3 => (e, rest3) //this is correct; do nothing.
          case _ => throw new StatementParseException(") expected.")
        }
      }
      case LBrack +: rest =>
        val (arr, rest2) = parseArray(rest)
        rest2 match{
          case LBrack +: rest3 => parseArrayOps(arr, rest3)
          case _ => (arr, rest2)
        }
      case _ => throw new StatementParseException("Invalid symbol: " + toks.head)
    }
    
    def parseExpr1(toks: List[Token]): (Expr, List[Token]) = {
      //Exponentiation.
      //Note: exponentiation gets handled right to left.
      //Recursive: <expr1> = <expr0> "**" <expr1> | <expr0>
      //Iterative: <expr1> = <expr0> ("**" <expr1>)*
      val (base, toks2) = parseExpr0(toks)
      toks2 match{
        case Ast +: Ast +: rest =>
          val (exp, toks3) = parseExpr1(toks2.tail.tail)
          (Pow(base, exp), toks3)
        case _ => (base, toks2)
      }
    }
    
    def parseExpr2(toks: List[Token]): (Expr, List[Token]) = {
      //Multiplication, division, divide and truncate, modulo.
      //Recursive: <expr2> = <expr2> "*" <expr1> | <expr1>
      //Iterative: <expr2> = <expr1> ("*" <expr1>)*
      var (left, toks2) = parseExpr1(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Ast +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = Mult(left, right)
            toks2 = toks3 }
          case Slash +: Slash +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = DivTrunc(left, right)
            toks2 = toks3 }
          case Slash +: rest => {
            val (right, toks3) = parseExpr1(rest)
            left = Div(left, right)
            toks2 = toks3 }
          case Percent +: rest =>{
            val (right, toks3) = parseExpr1(rest)
            left = Mod(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr3(toks: List[Token]): (Expr, List[Token]) = {
      //Addition, subtraction.
      //Recursive: <expr3> = <expr3> "+" <expr2> | <expr2>
      //Iterative: <expr3> = <expr2> ("+" <expr2>)*
      var (left, toks2) = parseExpr2(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Plus +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = Add(left, right)
            toks2 = toks3 }
          case Minus +: rest => {
            val (right, toks3) = parseExpr2(rest)
            left = Sub(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr4(toks: List[Token]): (Expr, List[Token]) = {
      //Not.
      //Note: using the recursive definition here is not problematic since there is no left recursion.
      //Recursive: <expr4> = "not" <expr4> | <expr3>
      //Iterative: <expr4> = ("not")* <expr3>
      if(toks.headOption == Some(NotTok)) {
        val (e, toks2) = parseExpr4(toks.tail)
        (Not(e), toks2)
      }
      else parseExpr3(toks)
    } 
    
    def parseExpr5(toks: List[Token]): (Expr, List[Token]) = {
      //Comparators.
      //Recursive: <expr5> = <expr5> "==" <expr4> | <expr4>
      //Iterative: <expr5> = <expr4> ("==" <expr4>)*
      var (left, toks2) = parseExpr4(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case Equals +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = IsEqual(left, right)
            toks2 = toks3 }
          case Exclamation +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = NotEqual(left, right)
            toks2 = toks3 }
          case LAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = LessThanEqual(left, right)
            toks2 = toks3 }
          case RAngle +: Equals +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = GreaterThanEqual(left, right)
            toks2 = toks3 }
          case LAngle +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = LessThan(left, right)
            toks2 = toks3 }
          case RAngle +: rest => {
            val (right, toks3) = parseExpr4(rest)
            left = GreaterThan(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr6(toks: List[Token]): (Expr, List[Token]) = {
      //And.
      //Recursive: <expr6> = <expr6> "and" <expr5> | <expr5>
      //Iterative: <expr5> = <expr5> ("and" <expr5>)*
      var (left, toks2) = parseExpr5(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case AndTok +: rest => {
            val (right, toks3) = parseExpr5(rest)
            left = And(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseExpr7(toks: List[Token]): (Expr, List[Token]) = {
      //Or.
      //Recursive: <expr7> = <expr7> "or" <expr6> | <expr6>
      //Iterative: <expr7> = <expr6> ("or" <expr6>)*
      var (left, toks2) = parseExpr6(toks)
      var exit = false
      while(!exit){
        toks2 match{
          case OrTok +: rest => {
            val (right, toks3) = parseExpr6(rest)
            left = Or(left, right)
            toks2 = toks3 }
          case _ => exit = true
        }
      }
      (left, toks2)
    }
    
    def parseArgs(toks: List[Token]): (Args, List[Token]) = toks match{
      //Parses a series of arguments until it hits an RParen token.
      //Returns the compiled Args and the unconsumed tokens.
      case RParen +: rest => (Args(List.empty[Expr]), rest)
      case _ => {
        val (arg, rest) = parseExpr(toks)
        rest match{
          case RParen +: rest => (Args(List(arg)), rest)
          case Comma +: rest => {
            val (args, rest2) = parseArgs(rest)
            (Args(arg +: args.params), rest2)
          }
        }
      }
    }
    
    def parseTypedArgs(toks: List[Token]): (TypedArgs, List[Token]) = {
      //Parses a series of typed arguments until it hits an RParen token.
      //Returns the compiled TypedArgs and the unconsumed tokens.
      def parseHelp(toks: List[Token], callByRef: Boolean, t: Type): (TypedArgs, List[Token]) = toks match{
        case Symbol(s2) +: RParen +: rest => (TypedArgs(List(Arg(t, callByRef, Ident(s2)))), rest)
        case Symbol(s2) +: Comma +: Symbol(s3) +: rest =>
          val (args, rest2) = parseTypedArgs(Symbol(s3) +: rest)
          (TypedArgs(Arg(t, callByRef, Ident(s2)) +: args.params), rest2)
      }
      val callByRef = (toks.headOption == Some(Tilde))
      toks match{
        case RParen +: rest => (TypedArgs(List.empty[Arg]), rest)
        case Tilde +: Symbol(s1) +: rest =>
          val (t, rest2) = parseType(Symbol(s1) +: rest)
          parseHelp(rest2, callByRef, t)
        case Symbol(s1) +: rest =>
          val (t, rest2) = parseType(Symbol(s1) +: rest)
          parseHelp(rest2, callByRef, t)
      }
    }
    
    def parseBlockStatements(toks: List[Token]): (BlockStatement, List[Token]) = toks match{
      //Parses a series of statements until it hits an RCurly token.
      //Returns the compiled Statements and the unconsumed tokens.
      //Begins immediately after first LCurly.
      case RCurly +: rest => (BlockStatement(List.empty[Statement]), rest)
      case _ => {
        val (s, rest) = parseStatement(toks)
        val (block, rest2) = parseBlockStatements(rest)
        (BlockStatement(s +: block.stmts), rest2)
      }
    }
    
    def parseStructDec(toks: List[Token]): (DecStruct, List[Token]) = {
      //Parses a StructDec and returns a list of unconsumed Tokens.
      //Begins after the StructTok.
      def parseStructDecTypeParams(toks: List[Token]): (Option[List[Type]], List[Token]) = toks match{
        case Symbol(s1) +: LAngle +: rest =>
          val (params, rest2) = parseTypeParams(rest)
          (Some(params), Symbol(s1) +: rest2)
        case _ => (None, toks)
      }
      val (typeParams, rest) = parseStructDecTypeParams(toks)
      rest match{
      case Symbol(s1) +: LCurly +: rest =>
        var decList = List.empty[Dec]
        var rest2 = rest
        var flag = true
        while(rest2.headOption.nonEmpty && flag){
          try{
            val (dec, rest3) = parseDec(rest2)
            val rest4 = semicolonChecker(rest3)
            if(dec.isInstanceOf[DecFunct]) throw new StatementParseException("Structs cannot contain function declarations.")
            decList +:= dec //prepend; must reverse later
            rest2 = rest4
          }
          catch{
            case MalformedDeclarationException(msg) => flag = false
          }
        }
        rest2 match{
          case RCurly +: rest3 => (DecStruct(UserClassType(s1, typeParams), Ident(s1), decList.reverse), rest3)
          case _ => throw new MalformedDeclarationException("} expected.")
        }
      case _ => throw new MalformedDeclarationException("Improper syntax for struct declaration." + rest)
    }
    }
    
    def parseDec(toks: List[Token]): (Dec, List[Token]) = {
      //Parses a Dec according to the CFG and returns that Dec and a list of the unconsumed tokens.
      //A correctly formed Dec is expected; if one is not found, this will throw an exception.
      //DecFunct(Type(s1), Ident(s2), args, body)
      val (t, newToks) = parseType(toks)
      newToks match{
      case Symbol(s1) +: LParen +: rest => {
        //Extract typed args:
        val (typedArgs, rest2) = parseTypedArgs(rest)
        //Extract statements:
        if(rest2.headOption != Some(LCurly)) throw new StatementParseException("{ expected.")
        val (body, rest3) = parseBlockStatements(rest2.tail)
        //Determine function type:
        (DecFunct(FunctType(typedArgs.params.map(arg => arg.t), t), Ident(s1), typedArgs, body), SemiColon +: rest3)
      } 
      case Symbol(s1) +: Equals +: NewTok +: Symbol(s2) +: LParen +: rest => throw new NotImplementedException("Object oriented functionality not yet implemented.")
      case Symbol(s1) +: Equals +: rest => {
        val (value, rest2) = parseExpr(rest)
        (DecVar(t, Ident(s1), Some(value)), rest2)
      }
      case Symbol(s1) +: rest => (DecVar(t, Ident(s1), None), rest)
      case _ => throw MalformedDeclarationException("Improper declaration syntax.")
    }
    }
    
    def parseType(toks: List[Token]): (Type, List[Token]) = toks match{
      //Parses a Type and returns a List of unconsumed tokens.
      case Symbol(s1) +: LAngle +: rest =>
        //Parse a variable number of Types
        val (tList, rest2) = parseTypeParams(rest)
        if(s1 == "Array" && tList.length == 1) (ArrayType(tList.head), rest2)
        else if(s1 == "Array") throw new TypeException("Array takes only 1 type parameter.")
        else (UserClassType(s1, Some(tList)), rest2)
      case Symbol(s1) +: rest => (determineType(s1), rest)
      case _ => throw new MalformedDeclarationException("Invalid symbol in type declaration.")
    }
    
    def parseTypeParams(toks: List[Token]): (List[Type], List[Token]) = {
      //Parses parameterized types between < and >.
      //Begins after <.
      if(toks.headOption == Some(RAngle)) throw new MalformedDeclarationException("Type parameters must have more than 0 arguments.")
      var tList = ListBuffer.empty[Type]
      var rest = toks
      while(true){
        val (nextType, rest2) = parseType(rest)
        tList = tList :+ nextType
        rest2 match{
          case Comma +: rest3 => rest = rest3
          case RAngle +: rest3 => return (tList.toList, rest3)
          case _ => throw new MalformedDeclarationException("Invalid symbol in parameterized type declaration: " + rest.head)
        }
      }
      //This statement is unreachable, but is included for type checking purposes:
      throw new MalformedDeclarationException("Invalid symbol in parameterized type declaration.")
    }
    
    def determineType(name: String): Type = name match{
      //Returns the Type object associated with the given String.
      case "int" => IntType
      case "float" => FltType
      case "String" => StrType
      case "char" => ChrType
      case "bool" => BoolType
      case "void" => NullType
      case _ => UserClassType(name, None)
    }
    
    parseAll(tokens)
  }
}