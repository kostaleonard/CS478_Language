package lang

import scala.io.Source

/**
 * @author x74617
 */
object Main {  
  def main(args: Array[String]){    
    if(args.length == 1){
      val filename = args.head
      runProgram(filename)
      return
    }
    new Typechecker().runTests
    new Interpreter().runTests
    runProgram("src/files/Sample.cop")
    //runProgram("src/files/Mergesort.cop")
    //runProgram("src/files/Nodes.cop")
    //runProgram("src/files/BinarySearch.cop")
    //runProgram("src/files/Recursion.cop")
  }
  
  def runProgram(filename: String){
    val text = getTextFromFile(filename)
    val tokens = scan(text)
    //println(tokens)
    val parsed = parse(tokens)
    //println(parsed)
    typecheck(parsed)
    val interpreted = interpret(parsed)
    printOutput(filename, interpreted)
  }
  
  def printOutput(filename: String, output: List[Value]){
    //println(filename + " output:")
    output.foreach(println)
  }
  
  def getTextFromFile(filename: String): String = {
    Source.fromFile(filename).getLines.mkString("", "\n", "")
  }
  
  def scan(text: String): List[Token] = {
    new Scanner().scanner(text)
  }
  
  def parse(tokens: List[Token]): List[Statement] = {
    new Parser().parser(tokens)
  }
  
  def typecheck(program: List[Statement]): Unit = {
    //This will throw an Exception if the types do not match.
    new Typechecker().typecheck(program)
  }
  
  def interpret(program: List[Statement]): List[Value] = {
    new Interpreter().interpret(program)
  }
}