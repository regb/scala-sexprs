package sexprs

import Tokens._
import SExprs._

class Parser(lexer: Lexer) {

  import Parser._

  private var _currentToken: Token = null
  /* lookAhead token is Some(null) if we reached eof */
  private var _lookAhead: Option[Token] = None

  //return a next token or null if EOF
  private def nextToken: Token = {
    _lookAhead match {
      case Some(t) => {
        _lookAhead = None
        _currentToken = t
        t
      }
      case None => {
        _currentToken = lexer.nextToken
        _lookAhead = None
        _currentToken
      }
    }
  }

  /*
   * return the look ahead token, or null if EOF
   * Note: do not throw an exception as it is okay to lookahead into EOF
   */
  private def peekToken: Token = {
    _lookAhead match {
      case Some(t) => t
      case None =>
        _lookAhead = Some(lexer.nextToken)
        _lookAhead.get
    }
  }

  /*
   * Make sure the next token corresponds to t and read
   */
  private def eat(expected: Token): Unit = {
    val token = nextToken

    //If the tokens do not correspond this must be due to an internal bug, hence the assert
    assert(token == expected)
  }

 /* 
  * Return the next SExpr if there is one, or null if EOF.
  * Throw an Exception if EOF is reached at an unexpected moment (incomplete SExpr).
  */
  def parse: SExpr = {
    val tok = nextToken
    if(tok == null) null else {
      val expr = tok match {
        case OParen() => {
          val buffer = new scala.collection.mutable.ListBuffer[SExpr]
          while(peekToken != CParen()) {
            if(peekToken == null)
              throw new EOFBeforeMatchingParenthesisException(tok.getPos)
            buffer.append(this.parse)
          }
          eat(CParen())
          SList(buffer.toList)
        }
        case IntLit(d) => SInt(d)
        case StringLit(s) => SString(s)
        case SymbolLit(s) => s match { //NOTE: maybe we should have a BooleanLit as well
          case "true" => SBoolean(true)
          case "false" => SBoolean(false)
          case s => SSymbol(s)
        }
        case QualifiedSymbol(o, s) => SQualifiedSymbol(o.map(SSymbol), SSymbol(s))
        case DoubleLit(d) => SDouble(d)
        case CParen() => throw new UnexpectedTokenException(CParen(), tok.getPos)
      }
      expr.setPos(tok)
    }
  }


}

object Parser {

  class EOFBeforeMatchingParenthesisException(startPos: Position) extends
    Exception("Opened parenthesis at position: " + startPos + " has no matching closing parenthesis")
  class UnexpectedTokenException(token: Token, pos: Position) extends
    Exception("Unexpected token: " + token + " at position: " + pos)

  def fromString(str: String): Parser = {
    val lexer = new Lexer(new java.io.StringReader(str))
    new Parser(lexer)
  }

  def fromReader(reader: java.io.Reader): Parser = {
    val lexer = new Lexer(reader)
    new Parser(lexer)
  }

  /*
   * Parse a string and return the next SExpr in the string, ignore the rest
   */
  def exprFromString(str: String): SExpr = {
    val lexer = new Lexer(new java.io.StringReader(str))
    val parser = new Parser(lexer)
    parser.parse
  }
}
