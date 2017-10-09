package lang

/**
 * @author x74617
 */
//Tokens
sealed trait Token
case object LParen extends Token 
case object RParen extends Token 
case object LBrack extends Token 
case object RBrack extends Token 
case object LAngle extends Token 
case object RAngle extends Token 
case object LCurly extends Token 
case object RCurly extends Token 
case object Comma extends Token
case object Dot extends Token
case object Tilde extends Token
case object Slash extends Token
case object Plus extends Token
case object Minus extends Token
case object Percent extends Token
case object Ast extends Token
case object Exclamation extends Token
case object Equals extends Token
case object Colon extends Token
case object SemiColon extends Token
case class Ent(value: Integer) extends Token with Expr with Value {
  //the name Int is taken
  override def toString: String = value.toString
}
case class Flt(value: Float) extends Token with Expr with Value {
  override def toString: String = value.toString
}
case class Str(name: String) extends Token with Expr with Value {
  //matches items between " and "
  override def toString: String = name
}
case class Chr(name: Char) extends Token with Expr with Value {
  //matches a single item between ' and '
  override def toString: String = name.toString
}
case class Bool(value: Boolean) extends Token with Expr with Value {
  //matches true or false
  override def toString: String = value.toString
}
case class Symbol(name: String) extends Token{
  override def toString: String = name
}
//Keyword tokens:
//The Token versions of keywords do not take arguments;
//it is up to the parser to determine these inputs.
case object AndTok extends Token
case object OrTok extends Token
case object NotTok extends Token
case object ForTok extends Token
case object IfTok extends Token
case object ElifTok extends Token
case object ElseTok extends Token
case object WhileTok extends Token
case object ReturnTok extends Token
case object ContinueTok extends Token
case object BreakTok extends Token
case object NewTok extends Token
case object PrintTok extends Token
case object StructTok extends Token
case object Null extends Token with Expr with Value {
  override def toString: String = "null"
}

//Statements
sealed trait Expr extends Statement
//Already declared above:
//case class Ent(value: Integer) extends Token with Expr //the name Int is taken
//case class Flt(value: Float) extends Token with Expr
//case class Str(name: String) extends Token with Expr //matches items between " and "
//case class Chr(name: Char) extends Token with Expr //matches a single item between ' and '
//case class Bool(value: Boolean) extends Token with Expr //matches true or false
case class ArrExpr(contents: List[Expr]) extends Expr
case class ArrAccess(arr: Expr, index: Expr) extends Expr
case class ArrSlice(arr: Expr, start: Option[Expr], end: Option[Expr]) extends Expr
case class Ident(name: String) extends Expr {
  override def toString: String = name
}
case class Pow(base: Expr, exp: Expr) extends Expr
case class Mult(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class DivTrunc(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Not(expr: Expr) extends Expr
case class IsEqual(left: Expr, right: Expr) extends Expr
case class NotEqual(left: Expr, right: Expr) extends Expr
case class LessThanEqual(left: Expr, right: Expr) extends Expr
case class GreaterThanEqual(left: Expr, right: Expr) extends Expr
case class LessThan(left: Expr, right: Expr) extends Expr
case class GreaterThan(left: Expr, right: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr
case class FunctCall(child: Option[Expr], ident: Ident, args: Args) extends Expr
case class Field(child: Option[Expr], ident: Ident) extends Expr

sealed trait Dec extends Statement
case class DecVar(t: Type, i: Ident, value: Option[Expr] = None) extends Dec
case class DecFunct(t: FunctType, i: Ident, args: TypedArgs, body: BlockStatement) extends Dec
case class DecStruct(t: UserClassType, i: Ident, fields: List[Dec]) extends Dec
  
sealed trait Statement
case class Output(expr: Expr) extends Statement
case class Reassign(variable: Field, expr: Expr) extends Statement
case class ArrayMod(arrVar: Ident, access: ArrAccess, newValue: Expr) extends Statement
case class BlockStatement(stmts: List[Statement]) extends Statement
case class If(condition: Expr, body: Statement, otherwise: Option[If]) extends Statement
case class While(condition: Expr, body: Statement) extends Statement
case class For(dec: Option[DecVar], condition: Expr, change: Reassign, body: Statement) extends Statement
case class Return(value: Option[Expr] = None) extends Statement
case object Break extends Statement
case object Continue extends Statement

//Types
sealed trait Type
case object IntType extends Type{
  override def toString: String = "int"
}
case object FltType extends Type{
  override def toString: String = "float"
}
case object StrType extends Type{
  override def toString: String = "String"
}
case object ChrType extends Type{
  override def toString: String = "char"
}
case object BoolType extends Type{
  override def toString: String = "bool"
}
case object NullType extends Type{
  override def toString: String = "void"
}
case object EmptyArrayType extends Type
case class ArrayType(typeParam: Type) extends Type{
  override def toString: String = "Array<" + typeParam + ">"
}
case class FunctType(inParams: List[Type], returnType: Type) extends Type{
  override def toString: String = inParams.mkString("(", ", ", ")") + " => " + returnType
}
case class UserClassType(name: String, typeParam: Option[List[Type]]) extends Type{
  override def toString: String = {
    if(typeParam.nonEmpty) name + typeParam.mkString("<", ", ", ">")
    else name
  }
}

case class Args(params: List[Expr])
case class Arg(t: Type, callByReference: Boolean, ident: Ident)
case class TypedArgs(params: List[Arg])

//Custom Exceptions: should implement more later if time.
case class LangException(msg: String) extends Exception(msg){
  //override def toString: String = 
}
case class MalformedDeclarationException(msg: String) extends Exception(msg)
case class StringScanException(msg: String) extends Exception(msg)
case class CharScanException(msg: String) extends Exception(msg)
case class InvalidCharacterException(msg: String) extends Exception(msg)
case class InvalidArgumentException(msg: String) extends Exception(msg)
case class StatementParseException(msg: String) extends Exception(msg)
case class NotImplementedException(msg: String) extends Exception(msg)
case class UnboundVariableException(msg: String) extends Exception(msg)
case class InvalidStatementException(msg: String) extends Exception(msg)
case class InvalidExpressionException(msg: String) extends Exception(msg)
case class ConversionException(msg: String) extends Exception(msg)
case class TypeException(msg: String) extends Exception(msg)

class Location(t: Type, value: Option[Value]){
  var typ = t
  var contents = value
}
//Values
sealed trait Value
//Already declared:
//case class Ent(value: Integer) extends Token with Expr with Value//the name Int is taken
//case class Flt(value: Float) extends Token with Expr with Value
//case class Str(name: String) extends Token with Expr with Value //matches items between " and "
//case class Chr(name: Char) extends Token with Expr with Value //matches a single item between ' and '
//case class Bool(value: Boolean) extends Token with Expr with Value //matches true or false
case class FunctVal(t: FunctType, args: TypedArgs, body: BlockStatement, var staticEnv: Map[String, Location]) extends Value {
  override def toString: String = "Function: "+ t.toString
}
case class Arr(var buf: scala.collection.mutable.ArrayBuffer[Value]) extends Value {
  override def toString: String = buf.mkString("[", ", ", "]")
}
case class StructVal(t: UserClassType, var fieldEnv: Map[String, Location]) extends Value {
  override def toString: String = t.name
}

//Program Counter
//The program counter 
case class ProgramCounter(counterList: List[Int])