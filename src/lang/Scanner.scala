package lang

/**
 * @author x74617
 */
class Scanner {
  def scanner(text: String): List[Token] = {
    var programCounter = 1
    def scan(index: Int): List[Token] = {
      var i = index
      if(i == text.length) return List.empty[Token]
      val c = text(i)
      if(c == '(') LParen +: scan(i+1)
      else if(c == ')') RParen +: scan(i+1)
      else if(c == '[') LBrack +: scan(i+1)
      else if(c == ']') RBrack +: scan(i+1)
      else if(c == '<') LAngle +: scan(i+1)
      else if(c == '>') RAngle +: scan(i+1)
      else if(c == '{') LCurly +: scan(i+1)
      else if(c == '}') RCurly +: scan(i+1)
      else if(c == ',') Comma +: scan(i+1)
      else if(c == '.') {
        if(i+1 == text.length || !text(i+1).isDigit) Dot +: scan(i+1)
        else scanDigits(i)
      }
      else if(c == '~') Tilde +: scan(i+1)
      else if(c == '/') Slash +: scan(i+1)
      else if(c == '+') Plus +: scan(i+1)
      else if(c == '-') Minus +: scan(i+1)
      else if(c == '%') Percent +: scan(i+1)
      else if(c == '*') Ast +: scan(i+1)
      else if(c == '!') Exclamation +: scan(i+1)
      else if(c == '=') Equals +: scan(i+1)
      else if(c == ':') Colon +: scan(i+1)
      else if(c == ';') SemiColon +: scan(i+1)
      else if(c.isDigit) scanDigits(i)
      else if(c == '\"') scanStr(i)
      else if(c == '\'') scanChr(i)
      else if(c == '#') scanComment(i)
      else if(c.isLetter) scanLettersAndNumbers(i)     
      else if(c.isWhitespace) scan(i+1)
      else throw new InvalidCharacterException("Invalid char: " + c)
    }
    
    def scanComment(index: Int): List[Token] = {
      //Skips over the comment that begins at index.
      //Returns the scan of the character that follows the token.
      //For single line comments (#), skips to next new line.
      if(text(index) == '#'){
        val nextIndex = text.indexOf("\n", index+1)
        //If no new line was found.
        //In other words, the end of the file was reached, so there is no need to continue scanning.
        if(nextIndex == -1) List.empty
        else scan(nextIndex + 1)
      }
      else ??? //Block comments not implemented.
    }
    
    def scanDigits(index: Int): List[Token] = {
      //Returns an Ent or Flt concatenated with the scan of the rest of the text.
      //Returns Ent or Flt depending on the CFG rules:
      //<int> = (-?[0-9]*)|0
      //<flt> = (-?[0-9]+"."[0-9]*)|(-?[0-9]*"."[0-9]+)
      //Note that minus is handled by the Parser (works fine, but would be preferable to handle here.
      //Refactor if there is time later.
      var i = index
      var num = ""
      while(i < text.length && (text(i).isDigit || (text(i) == '.' && !num.contains('.')))){
        num += text(i)
        i += 1
      }
      if(num.contains('.')) Flt(num.toFloat) +: scan(i)
      else Ent(num.toInt) +: scan(i)
    }
    
    def scanStr(index: Int): List[Token] = {
      //Returns a Str concatenated with the scan of the rest of the text.
      //A Str is enclosed by " and ".
      //<str> = \"[^\"\n]\"
      
      //Because the character at index is a " (guaranteed by scan function), it can be skipped.
      var i = index + 1
      while(i < text.length && (text(i) != '\"')){
        //if(text(i) == '\n') throw new Exception("Literal newline character forbidden inside string.")
        i += 1
      }
      if(i >= text.length) throw new StringScanException("Expected closing \".")
      val s = text.substring(index + 1, i)
      Str(s) +: scan(i+1)
    }
    
    def scanChr(index: Int): List[Token] = {
      //Returns a Chr concatenated with the scan of the rest of the text.
      //A Chr is enclosed by ' and '.
      //<chr> = \'[^\'\n]\' 
      
      //Because the character at index is a ' (guaranteed by scan function), it can be skipped.
      val i = index + 1
      //Does this handle escaped characters? Are those counted as one or two characters?
      if(i >= text.length - 1) throw new CharScanException("Expected closing \'.")
      val c = text(i)
      if(text(i+1) != '\'') throw new CharScanException("Char must be of length 1.")
      Chr(c) +: scan(i+2)
    }
    
    def scanLettersAndNumbers(index: Int): List[Token] = {
      //Collects all of the letters starting at index in sequence.
      //Ends when a non-letter/non-number/non-underscore character is encountered.
      //Must begin with a letter.
      var i = index
      var word = ""
      while(i < text.length && (text(i).isLetterOrDigit || text(i) == '_')){
        word += text(i)
        i += 1
      }
      //Return the correct Token based on the completed word.
      if(word == "true") Bool(true) +: scan(i)
      else if(word == "false") Bool(false) +: scan(i)
      else if(word == "and") AndTok +: scan(i)
      else if(word == "or") OrTok +: scan(i)
      else if(word == "not") NotTok +: scan(i)
      else if(word == "for") ForTok +: scan(i)
      else if(word == "if") IfTok +: scan(i)
      else if(word == "elif") ElifTok +: scan(i)
      else if(word == "else") ElseTok +: scan(i)
      else if(word == "while") WhileTok +: scan(i)
      else if(word == "return") ReturnTok +: scan(i)
      else if(word == "continue") ContinueTok +: scan(i)
      else if(word == "break") BreakTok +: scan(i)
      else if(word == "new") NewTok +: scan(i)
      else if(word == "print") PrintTok +: scan(i)
      else if(word == "struct") StructTok +: scan(i)
      else if(word == "null") Null +: scan(i)
      else Symbol(word) +: scan(i)
    }
    
    scan(0)
  }
}