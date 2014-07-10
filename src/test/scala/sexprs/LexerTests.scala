package sexprs

import Tokens._

import java.io.StringReader

import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class LexerTests extends FunSuite with Timeouts {

  override def suiteName = "S-expression Lexer suite"

  test("eof read") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === IntLit(12))
    assert(lexer1.nextToken === null)
  }

  test("integer literals") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === IntLit(12))

    val reader2 = new StringReader("#xF")
    val lexer2 = new Lexer(reader2)
    assert(lexer2.nextToken === IntLit(15))

    val reader3 = new StringReader("#o55")
    val lexer3 = new Lexer(reader3)
    assert(lexer3.nextToken === IntLit(45))

    val reader4 = new StringReader("#x1F")
    val lexer4 = new Lexer(reader4)
    assert(lexer4.nextToken === IntLit(31))

    val reader5 = new StringReader("123 #x11 #o12")
    val lexer5 = new Lexer(reader5)
    assert(lexer5.nextToken === IntLit(123))
    assert(lexer5.nextToken === IntLit(17))
    assert(lexer5.nextToken === IntLit(10))

    val reader6 = new StringReader("#16r21")
    val lexer6 = new Lexer(reader6)
    assert(lexer6.nextToken === IntLit(33))
  }

  test("string literals") {
    val reader1 = new StringReader(""" "12" """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === StringLit("12"))

    val reader2 = new StringReader(""" "abc\"def" """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.nextToken === StringLit("abc\"def"))

    val reader3 = new StringReader(""" " abc \" def" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.nextToken === StringLit(" abc \" def"))

    val reader4 = new StringReader(""" "\"abc\"" """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.nextToken === StringLit("\"abc\""))
  }


  test("symbol literals") {
    val reader1 = new StringReader(""" d12 """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === SymbolLit("d12"))

    val reader2 = new StringReader(""" abc\ def """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.nextToken === SymbolLit("abc def"))

    val reader3 = new StringReader("""  ab\c\ d\ef" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.nextToken === SymbolLit("abc def"))

    val reader4 = new StringReader(""" |abc deF| """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.nextToken === SymbolLit("abc deF"))

    val reader5 = new StringReader(""" 
|abc
deF| 
""")
    val lexer5 = new Lexer(reader5)
    assert(lexer5.nextToken === SymbolLit(
"""abc
deF"""))
  }

  test("qualified symbols") {
    val reader1 = new StringReader(""" :d12 """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === QualifiedSymbol(None, "d12"))

    val reader2 = new StringReader(""" abc:def """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.nextToken === QualifiedSymbol(Some("abc"), "def"))

    val reader3 = new StringReader("""  ab\c:d\ef" """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.nextToken === QualifiedSymbol(Some("abc"), "def"))

    val reader4 = new StringReader(""" |abc : deF| """)
    val lexer4 = new Lexer(reader4)
    assert(lexer4.nextToken === SymbolLit("abc : deF"))

    val reader5 = new StringReader(""" |abc|:|deF| """)
    val lexer5 = new Lexer(reader5)
    assert(lexer5.nextToken === QualifiedSymbol(Some("abc"), "deF"))
  }

  test("lexer compose") {
    val reader1 = new StringReader("""
      (test "test")
    """)
    val lexer1 = new Lexer(reader1)
    assert(lexer1.nextToken === OParen())
    assert(lexer1.nextToken === SymbolLit("test"))
    assert(lexer1.nextToken === StringLit("test"))
    assert(lexer1.nextToken === CParen())


    val reader2 = new StringReader("""
      ) (  42  42.173
    """)
    val lexer2 = new Lexer(reader2)
    assert(lexer2.nextToken === CParen())
    assert(lexer2.nextToken === OParen())
    assert(lexer2.nextToken === IntLit(42))
    assert(lexer2.nextToken === DoubleLit(42.173))

    val reader3 = new StringReader("""
      ) ;(  42  42.173
      12 "salut" ; """)
    val lexer3 = new Lexer(reader3)
    assert(lexer3.nextToken === CParen())
    assert(lexer3.nextToken === IntLit(12))
    assert(lexer3.nextToken === StringLit("salut"))
  }

  test("interactive lexer") {
    val pis = new SynchronousPipedReader
    /*
     * Since the pipe is empty, the lexer should not even start to read
     * in the reader. It should only start reading when asked for the nextToken token.
     */
    val lexer = failAfter(3 seconds) { new Lexer(pis) }
    /* 
     * this is impossible for the lexer to determine whether the token is terminated 
     * or if the nextToken char takes time to arrive, so we need some syntactic separation
     * hence the space after 12
     */
    pis.write("12 ")
    assert(lexer.nextToken === IntLit(12))
    pis.write("(")
    assert(lexer.nextToken === OParen())
    pis.write(")")
    assert(lexer.nextToken === CParen())
    pis.write("\"abcd\"")
    assert(lexer.nextToken === StringLit("abcd"))
  }

  test("Positions of tokens") {
    val reader1 = new StringReader("12")
    val lexer1 = new Lexer(reader1)
    val token1 = lexer1.nextToken
    assert(token1 === IntLit(12))
    assert(token1.getPos == Position(1, 1))

    val reader2 = new StringReader("  12 ")
    val lexer2 = new Lexer(reader2)
    val token2 = lexer2.nextToken
    assert(token2 === IntLit(12))
    assert(token2.getPos == Position(1, 3))

    val reader3 = new StringReader("""(test "test")""")
    val lexer3 = new Lexer(reader3)
    val token31 = lexer3.nextToken
    val token32 = lexer3.nextToken
    val token33 = lexer3.nextToken
    val token34 = lexer3.nextToken
    assert(token31 === OParen())
    assert(token31.getPos === Position(1,1))
    assert(token32 === SymbolLit("test"))
    assert(token32.getPos === Position(1,2))
    assert(token33 === StringLit("test"))
    assert(token33.getPos === Position(1,7))
    assert(token34 === CParen())
    assert(token34.getPos === Position(1,13))

    val reader4 = new StringReader(
"""test
  12
 )""")
    val lexer4 = new Lexer(reader4)
    val token41 = lexer4.nextToken
    val token42 = lexer4.nextToken
    val token43 = lexer4.nextToken
    assert(token41 === SymbolLit("test"))
    assert(token41.getPos === Position(1,1))
    assert(token42 === IntLit(12))
    assert(token42.getPos === Position(2,3))
    assert(token43 === CParen())
    assert(token43.getPos === Position(3,2))
  }

  test("Positions of parenthesis") {
    val reader = new StringReader("()()()")
    val lexer = new Lexer(reader)
    val token1 = lexer.nextToken
    val token2 = lexer.nextToken
    val token3 = lexer.nextToken
    val token4 = lexer.nextToken
    val token5 = lexer.nextToken
    val token6 = lexer.nextToken
    assert(token1 === OParen())
    assert(token1.getPos == Position(1, 1))
    assert(token2 === CParen())
    assert(token2.getPos == Position(1, 2))
    assert(token3 === OParen())
    assert(token3.getPos == Position(1, 3))
    assert(token4 === CParen())
    assert(token4.getPos == Position(1, 4))
    assert(token5 === OParen())
    assert(token5.getPos == Position(1, 5))
    assert(token6 === CParen())
    assert(token6.getPos == Position(1, 6))
  }

}
