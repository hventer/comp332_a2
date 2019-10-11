/*
 * This file is part of COMP332 Assignment 2 2018.
 *
 * Lintilla, a simple functional programming language.
 *
 * © 2018, Dominic Verity and Anthony Sloane, Macquarie University.
 *         All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Tests of the parser of the Lintilla language.
 */

package lintilla

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Tests that check that the parser works correctly.  I.e., it accepts correct
  * input and produces the appropriate trees, and it rejects illegal input.
  */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

  import LintillaTree._

  val parsers = new SyntaxAnalysis(positions)
  import parsers._

  // Tests of parsing terminals

  test("parsing an identifier of one letter produces the correct tree") {
    identifier("x") should parseTo[String]("x")
  }

  test("parsing an identifier as an identifier produces the correct tree") {
    identifier("count") should parseTo[String]("count")
  }

  test("parsing an identifier containing digits and underscores produces the correct tree") {
    identifier("x1_2_3") should parseTo[String]("x1_2_3")
  }

  test("parsing an integer as an identifier gives an error") {
    identifier("42") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (digit)") {
    identifier("4foo") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '4' found")
  }

  test("parsing a non-identifier as an identifier gives an error (underscore)") {
    identifier("_f3") should
    failParseAt(1, 1,
                "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '_' found")
  }

  test("parsing a keyword as an identifier gives an error") {
    identifier("let ") should failParseAt(1, 1,
                                          "identifier expected but keyword found")
  }

  test("parsing a keyword prefix as an identifier produces the correct tree") {
    identifier("letter") should parseTo[String]("letter")
  }

  test("parsing an integer of one digit as an integer produces the correct tree") {
    integer("8") should parseTo[String]("8")
  }

  test("parsing an integer as an integer produces the correct tree") {
    integer("99") should parseTo[String]("99")
  }

  test("parsing a non-integer as an integer gives an error") {
    integer("total") should
    failParseAt(1, 1,
                "string matching regex '[0-9]+' expected but 't' found")
  }

  // Tests of parsing basic expressions

  test("parsing an equal expression produces the correct tree") {
    exp("a = 1") should parseTo[Expression](EqualExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing a less than expression produces the correct tree") {
    exp("a < 1") should parseTo[Expression](LessExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing an addition expression produces the correct tree") {
    exp("a + 1") should parseTo[Expression](PlusExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing a subtraction expression produces the correct tree") {
    exp("a - 1") should parseTo[Expression](MinusExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing a multiplication expression produces the correct tree") {
    exp("a * 1") should parseTo[Expression](StarExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing a division expression produces the correct tree") {
    exp("a / 1") should parseTo[Expression](SlashExp(IdnExp(IdnUse("a")), IntExp(1)))
  }

  test("parsing an integer expression produces the correct tree") {
    exp("823") should parseTo[Expression](IntExp(823))
  }

  test("parsing a true expression produces the correct tree") {
    exp("true") should parseTo[Expression](BoolExp(true))
  }

  test("parsing a false expression produces the correct tree") {
    exp("false") should parseTo[Expression](BoolExp(false))
  }

  test("parsing an identifier expression produces the correct tree") {
    exp("v123") should parseTo[Expression](IdnExp(IdnUse("v123")))
  }

  test("parsing a parenthesized expression produces the correct tree") {
    exp("(a + 5)") should parseTo[Expression](PlusExp(IdnExp(IdnUse("a")), IntExp(5)))
  }

  test("parsing an application expression produces the correct tree") {
    exp("a(b)") should parseTo[Expression](
      AppExp(IdnExp(IdnUse("a")), Vector(IdnExp(IdnUse("b")))))
  }

  test("parsing another application expression produces the correct tree") {
    exp("a(b,c)(10,12)") should parseTo[Expression](
      AppExp(
        AppExp(
          IdnExp(IdnUse("a")),
          Vector(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))),
        Vector(IntExp(10), IntExp(12))))
  }

  // Tests of binary operator associativity

  test("< is not associative") {
    phrase(exp)("a < b < c") should
    failParseAt(1, 7, "'-' expected but '<' found")
  }

  test("= is not associative") {
    phrase(exp)("a = b = c") should
    failParseAt(1, 7, "'-' expected but '=' found")
  }

  test("= and < are not associative") {
    phrase(exp)("a = b < c") should
    failParseAt(1, 7, "'-' expected but '<' found")
  }

  test("+ is left associative") {
    phrase(exp)("a + b + c") should parseTo[Expression](
      PlusExp(PlusExp(IdnExp(IdnUse("a")),
                      IdnExp(IdnUse("b"))),
              IdnExp(IdnUse("c"))))
  }

  test("- is left associative") {
    phrase(exp)("a - b - c") should parseTo[Expression](
      MinusExp(MinusExp(IdnExp(IdnUse("a")),
                        IdnExp(IdnUse("b"))),
               IdnExp(IdnUse("c"))))
  }

  test("- and + are left associative") {
    phrase(exp)("a + b - c") should parseTo[Expression](
      MinusExp(PlusExp(IdnExp(IdnUse("a")),
                        IdnExp(IdnUse("b"))),
               IdnExp(IdnUse("c"))))
  }

  test("* is left associative") {
    phrase(exp)("a * b * c") should parseTo[Expression](
      StarExp(StarExp(IdnExp(IdnUse("a")),
                      IdnExp(IdnUse("b"))),
              IdnExp(IdnUse("c"))))
  }

  test("/ is left associative") {
    phrase(exp)("a / b / c") should parseTo[Expression](
      SlashExp(SlashExp(IdnExp(IdnUse("a")),
                        IdnExp(IdnUse("b"))),
               IdnExp(IdnUse("c"))))
  }

  test("/ and * are left associative") {
    phrase(exp)("a / b * c") should parseTo[Expression](
      StarExp(SlashExp(IdnExp(IdnUse("a")),
                        IdnExp(IdnUse("b"))),
               IdnExp(IdnUse("c"))))
  }

  // Tests of function application relative precedence

  test("= has lower precedence than application (to left)") {
    phrase(exp)("a = b(c)") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("< has lower precedence than application (to left)") {
     phrase(exp)("a < b(c)") should parseTo[Expression](
       LessExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("+ has lower precedence than application (to left)") {
    phrase(exp)("a + b(c)") should parseTo[Expression](
      PlusExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("- has lower precedence than application (to left)") {
    phrase(exp)("a - b(c)") should parseTo[Expression](
      MinusExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("* has lower precedence than application (to left)") {
    phrase(exp)("a * b(c)") should parseTo[Expression](
      StarExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("/ has lower precedence than application (to left)") {
    phrase(exp)("a / b(c)") should parseTo[Expression](
      SlashExp(IdnExp(IdnUse("a")), AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  test("= has lower precedence than application (to right)") {
    phrase(exp)("b(c) = a") should parseTo[Expression](
      EqualExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("< has lower precedence than application (to right)") {
    phrase(exp)("b(c) < a") should parseTo[Expression](
      LessExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("+ has lower precedence than application (to right)") {
    phrase(exp)("b(c) + a") should parseTo[Expression](
      PlusExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("- has lower precedence than application (to right)") {
    phrase(exp)("b(c) - a") should parseTo[Expression](
      MinusExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("* has lower precedence than application (to right)") {
    phrase(exp)("b(c) * a") should parseTo[Expression](
      StarExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("/ has lower precedence than application (to right)") {
    phrase(exp)("b(c) / a") should parseTo[Expression](
      SlashExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c")))), IdnExp(IdnUse("a"))))
  }

  test("test negation of function application") {
    phrase(exp)("-b(c)") should parseTo[Expression](
      NegExp(AppExp(IdnExp(IdnUse("b")), Vector(IdnExp(IdnUse("c"))))))
  }

  // Tests of arithmetic operator relative precedence.

  test("+ has lower precedence than * (to left)") {
    exp("a + b * c") should parseTo[Expression](
      PlusExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("- has lower precedence than * (to left)") {
    exp("a - b * c") should parseTo[Expression](
      MinusExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("+ has lower precedence than * (to right)") {
    exp("b * c + a") should parseTo[Expression](
      PlusExp(StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("- has lower precedence than * (to right)") {
    exp("b * c - a") should parseTo[Expression](
      MinusExp(StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("+ has lower precedence than / (to left)") {
    exp("a + b / c") should parseTo[Expression](
      PlusExp(IdnExp(IdnUse("a")), SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("- has lower precedence than / (to left)") {
    exp("a - b / c") should parseTo[Expression](
      MinusExp(IdnExp(IdnUse("a")), SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("+ has lower precedence than / (to right)") {
    exp("b / c + a") should parseTo[Expression](
      PlusExp(SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("- has lower precedence than / (to right)") {
    exp("b / c - a") should parseTo[Expression](
      MinusExp(SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  // Tests of relative precedence of relations

  test("= has lower precedence than * (to left)") {
    exp("a = b * c") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("< has lower precedence than * (to left)") {
    exp("a < b * c") should parseTo[Expression](
      LessExp(IdnExp(IdnUse("a")), StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("= has lower precedence than * (to right)") {
    exp("b * c = a") should parseTo[Expression](
      EqualExp(StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("< has lower precedence than * (to right)") {
    exp("b * c < a") should parseTo[Expression](
      LessExp(StarExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("= has lower precedence than / (to left)") {
    exp("a = b / c") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("< has lower precedence than / (to left)") {
    exp("a < b / c") should parseTo[Expression](
      LessExp(IdnExp(IdnUse("a")), SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("= has lower precedence than / (to right)") {
    exp("b / c = a") should parseTo[Expression](
      EqualExp(SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("< has lower precedence than / (to right)") {
    exp("b / c < a") should parseTo[Expression](
      LessExp(SlashExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("= has lower precedence than + (to left)") {
    exp("a = b + c") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("< has lower precedence than + (to left)") {
    exp("a < b + c") should parseTo[Expression](
      LessExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("= has lower precedence than + (to right)") {
    exp("b + c = a") should parseTo[Expression](
      EqualExp(PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("< has lower precedence than + (to right)") {
    exp("b + c < a") should parseTo[Expression](
      LessExp(PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("= has lower precedence than - (to left)") {
    exp("a = b - c") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), MinusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("< has lower precedence than - (to left)") {
    exp("a < b - c") should parseTo[Expression](
      LessExp(IdnExp(IdnUse("a")), MinusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("= has lower precedence than - (to right)") {
    exp("b - c = a") should parseTo[Expression](
      EqualExp(MinusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  test("< has lower precedence than - (to right)") {
    exp("b - c < a") should parseTo[Expression](
      LessExp(MinusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c"))), IdnExp(IdnUse("a"))))
  }

  // Tests of unary minus precedence.

  test("unary - has higher precedence than = (to left)") {
    exp("-a = b") should parseTo[Expression](
      EqualExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence than = (to right)") {
    exp("a = -b") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  test("unary - has higher precedence that < (to left)") {
    exp("-a < b") should parseTo[Expression](
      LessExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence that < (to right)") {
    exp("a < -b") should parseTo[Expression](
      LessExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  test("unary - has higher precedence than + (to left)") {
    exp("-a + b") should parseTo[Expression](
      PlusExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence than + (to right)") {
    exp("a + -b") should parseTo[Expression](
      PlusExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  test("unary - has higher precedence than - (to left)") {
    exp("-a - b") should parseTo[Expression](
      MinusExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence than - (to right)") {
    exp("a - -b") should parseTo[Expression](
      MinusExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  test("unary - has higher precedence than * (to left)") {
    exp("-a * b") should parseTo[Expression](
      StarExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence than * (to right)") {
    exp("a * -b") should parseTo[Expression](
      StarExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  test("unary - has higher precedence than / (to left)") {
    exp("-a / b") should parseTo[Expression](
      SlashExp(NegExp(IdnExp(IdnUse("a"))), IdnExp(IdnUse("b"))))
  }

  test("unary - has higher precedence than / (to right)") {
    exp("a / -b") should parseTo[Expression](
      SlashExp(IdnExp(IdnUse("a")), NegExp(IdnExp(IdnUse("b")))))
  }

  // Parenthesis tests.

  test("parentheses override precedence (to left)") {
    exp("(a + b) * c") should parseTo[Expression](
      StarExp(PlusExp(IdnExp(IdnUse("a")), IdnExp(IdnUse("b"))), IdnExp(IdnUse("c"))))
  }

  test("parentheses override precedence (to right)") {
    exp("a * (b + c)") should parseTo[Expression](
      StarExp(IdnExp(IdnUse("a")), PlusExp(IdnExp(IdnUse("b")), IdnExp(IdnUse("c")))))
  }

  test("parentheses override associativity in expressions") {
    exp("a + (b + c)") should parseTo[Expression](
      PlusExp(IdnExp(IdnUse("a")),
              PlusExp(IdnExp(IdnUse("b")),
                      IdnExp(IdnUse("c")))))
  }

  test("parentheses disambiguate non-associativity (to right)") {
    exp("a = (b = c)") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("a")),
               EqualExp(IdnExp(IdnUse("b")),
                        IdnExp(IdnUse("c")))))
  }

  test("parentheses disambiguate non-associativity (to left)") {
    exp("(a < b) < c") should parseTo[Expression](
      LessExp(
        LessExp(IdnExp(IdnUse("a")),
                IdnExp(IdnUse("b"))),
        IdnExp(IdnUse("c"))))
  }

  // Test parentheses and compound expressions.

  test("use of un-bracketed 'if' in arithmetic expression fails (to right)") {
    phrase(exp) ("1 + if true { 2 } else { 3 }") should
    failParseAt(1, 5, "'(' expected but 'i' found")
  }

  test("use of un-bracketed 'if' in arithmetic expression fails (to left)") {
    phrase(exp) ("if true { 2 } else { 3 } * 4") should
    failParseAt(1, 26, "end of input expected")
  }

  test("bracketed 'if' in arithmetic expression gives the right tree") {
    exp("10 + (if b { 2 } else { x }) * 4") should parseTo[Expression](
      PlusExp(IntExp(10),
              StarExp(IfExp(IdnExp(IdnUse("b")),
                            Block(Vector(IntExp(2))),
                            Block(Vector(IdnExp(IdnUse("x"))))),
                      IntExp(4))))
  }

  // Kym's binary / unary expression tests

  test("Kym's first precedence / associativity test") {
    phrase(exp) ("2 + 3 * 4 + 5") should parseTo[Expression](
      PlusExp(PlusExp(IntExp(2),StarExp(IntExp(3), IntExp(4))), IntExp(5)))
  }

  test("Kym's second precedence / associativity test") {
    phrase(exp) ("2 - 3 * 4 - 5") should parseTo[Expression](
      MinusExp(MinusExp(IntExp(2),StarExp(IntExp(3), IntExp(4))), IntExp(5)))
  }

  test("Kym's third precedence / associativity test") {
    phrase(exp) ("2 * - 3 * 5") should parseTo[Expression](
      StarExp(StarExp(IntExp(2),NegExp(IntExp(3))), IntExp(5)))
  }

  test("Kym's fourth precedence / associativity test") {
    phrase(exp) ("2 / - 3 / 5") should parseTo[Expression](
      SlashExp(SlashExp(IntExp(2),NegExp(IntExp(3))), IntExp(5)))
  }

  // Block expression tests

  test("parsing an empty block expression produces the correct tree") {
    exp("{}") should parseTo[Expression](Block(Vector()))
  }

  test("parsing a block containing one expression produces the correct tree") {
    exp("{ x }") should parseTo[Expression](Block(Vector(IdnExp(IdnUse("x")))))
  }

  test("parsing a block of ';' separated expressions produces the correct tree") {
    exp("{ x; 10 }") should parseTo[Expression](
      Block(Vector(IdnExp(IdnUse("x")), IntExp(10))))
  }

  test("parsing a block of many ';' separated expressions produces the correct tree") {
    exp("{ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 }") should parseTo[Expression](
      Block(Vector(
              IntExp(1), IntExp(2), IntExp(3), IntExp(4), IntExp(5), IntExp(6),
              IntExp(7), IntExp(8), IntExp(9), IntExp(10))))
  }

  test("parsing a block containing a hanging ';' fails") {
    exp("{ x; }") should failParseAt(1, 4, "'}' expected but ';' found")
  }

  // 'let' expression tests

  test("parsing a 'let' declaration produces the correct tree") {
    exp("let x = 1") should parseTo[Expression](
      LetDecl(IdnDef("x"), IntExp(1)))
  }

  test ("parsing a 'let' declaration missing an initialisation expression fails") {
    exp("let temp =") should failParseAt(1, 11, "'fn' expected but end of source found")
  }

  test ("parsing a 'let' declaration missing an '=' fails") {
    exp("let temp 10") should failParseAt(1, 10, "'=' expected but '1' found")
  }

  test ("parsing a 'let' declaration missing a variable fails") {
    exp("let = 10") should failParseAt(1, 5, "string matching regex '[a-zA-Z][a-zA-Z0-9_]*' expected but '=' found")
  }

  test ("parsing a 'let' with complex init expression produces the correct tree.") {
    exp("let letter = 10 + 3 * x / 3 - 11") should parseTo[Expression] (
      LetDecl(IdnDef("letter"),
              MinusExp(
                PlusExp(IntExp(10),
                        SlashExp(
                          StarExp(IntExp(3),
                                  IdnExp(IdnUse("x"))),
                          IntExp(3))),
                IntExp(11))))
  }

  test ("check that the keywords 'let' and 'mut' cannot be run together") {
    exp("letmuta = 1") should parseTo[Expression](
      EqualExp(IdnExp(IdnUse("letmuta")), IntExp(1)))
  }

  test ("check that the keywords 'let' and 'mut' cannot be run together (2)") {
    phrase(exp)("letmut a = 1") should failParseAt(1, 8, "'<' expected but 'a' found")
  }

  // If expression tests.

  test("parsing a trivial 'if' expression produces the correct tree") {
    exp("if false {} else {}") should parseTo[Expression](
      IfExp(BoolExp(false),
            Block(Vector()),
            Block(Vector())))
  }

  test("parsing an 'if' expression with a more complex condition produces the correct tree") {
    exp("if x + 10 = y {} else {}") should parseTo[Expression](
      IfExp(EqualExp(PlusExp(IdnExp(IdnUse("x")), IntExp(10)),IdnExp(IdnUse("y"))),
            Block(Vector()),
            Block(Vector())))
  }

  test ("parsing an 'if' with missing else block fails") {
    exp("if true {} else") should failParseAt(1, 16, "'{' expected but end of source found")
  }

  test ("parsing an 'if' with missing 'else' fails") {
    exp("if true {} {}") should failParseAt(1, 12, "'else' expected but '{' found")
  }

  test ("parsing an 'if' with missing then block fails") {
    exp("if true else {}") should failParseAt(1, 9, "'{' expected but 'e' found")
  }

  test ("parsing an 'if' with a missing conditions fails") {
    exp("if {} else {}") should failParseAt(1, 7, "'{' expected but 'e' found")
  }

  // Parameter declaration tests.

  test("parsing a parameter declaration of 'int' type produces the right tree") {
    paramDecl("dom: int") should parseTo[ParamDecl] (
      ParamDecl(IdnDef("dom"), IntType())
    )
  }

  test("parsing a parameter with 'fn' type produces the right tree") {
    paramDecl("dom : fn(int) -> int") should parseTo[ParamDecl](
      ParamDecl(IdnDef("dom"), FnType(Vector(IntType()),IntType()))
    )
  }

  test("parsing a parameter declaration with missing type fails") {
    paramDecl("x:") should failParseAt(1, 3, "'(' expected but end of source found")
  }

  test("parsing a parameter declatation with missing ':' fails") {
    paramDecl("dom int") should failParseAt(1, 5, "':' expected but 'i' found")
  }

  // Function declaration tests.

  test("parsing a 'fn' declaration produces the correct tree") {
    exp("fn f(x: int, y: int) -> int { (x+1)*x }") should parseTo[Expression](
      FnDecl(IdnDef("f"),
             Vector(
               ParamDecl(IdnDef("x"), IntType()),
               ParamDecl(IdnDef("y"), IntType())),
             Option(IntType()),
             Block(Vector(StarExp(PlusExp(IdnExp(IdnUse("x")), IntExp(1)),
                                  IdnExp(IdnUse("x")))))))
  }

  test("parsing function declaration without return value produces the correct tree") {
    exp("fn f(n : int) { }") should parseTo[Expression](
      FnDecl(IdnDef("f"),
             Vector(ParamDecl(IdnDef("n"), IntType())),
             None,
             Block(Vector())))
  }

  test("parsing a function declaration with an empty parameter list produces the correct tree") {
    exp("fn f() -> int { 10 }") should parseTo[Expression] (
      FnDecl(IdnDef("f"), Vector(), Some(IntType()), Block(Vector(IntExp(10))))
    )
  }

  test("parsing a function with empty parameter list and no return type produces the correct tree") {
    exp("fn f() {}") should parseTo[Expression] (
      FnDecl(IdnDef("f"), Vector(), None, Block(Vector()))
    )
  }

  // Tests of parsing types

  test("parsing the 'unit' type produces the correct tree") {
    tipe("unit") should parseTo[Type](UnitType())
  }

  test("parsing the 'int' type produces the correct tree") {
    tipe("int") should parseTo[Type](IntType())
  }

  test("parsing the 'bool' type produces the correct tree") {
    tipe("bool") should parseTo[Type](BoolType())
  }

  test("parsing a function type with no parameters produces the correct tree") {
    tipe("fn() -> int") should parseTo[Type](
      FnType(Vector(), IntType()))
  }

  test("parsing a function type with one parameter produces the correct tree") {
    tipe("fn(bool) -> int") should parseTo[Type](
      FnType(Vector(BoolType()), IntType()))
  }

  test("parsing a function type with three parameters produces the correct tree") {
    tipe("fn(bool, int, int) -> unit") should parseTo[Type](
      FnType(Vector(BoolType(), IntType(), IntType()), UnitType()))
  }

  test("parsing a function type with a parameter of function type produces the correct tree") {
    tipe("fn(bool, fn(int) -> int) -> int") should parseTo[Type](
      FnType(Vector(BoolType(), FnType(Vector(IntType()),IntType())),IntType())
    )
  }

  test("parsing a function type with a return type which is a function type produces the correct tree") {
    tipe("fn(bool, int) -> fn(int) -> unit") should parseTo[Type](
      FnType(Vector(BoolType(), IntType()),FnType(Vector(IntType()),UnitType()))
    )
  }

  test("parsing a function type with parameters of function type") {
    tipe("fn(fn(int) -> unit, fn(bool) -> int) -> unit") should parseTo[Type](
      FnType(Vector(
               FnType(Vector(IntType()), UnitType()),
               FnType(Vector(BoolType()), IntType())), UnitType()))
  }

  // Tests of parsing programs

  test("a program must have at least one expression") {
    program("") should failParseAt(1, 1, """'fn' expected but end of source found""")
  }

  test("a program with one expression produces the correct tree") {
    program("42") should parseTo[Program](Program(Vector(IntExp(42))))
  }

  test("a program with a few expressions produces the correct tree") {
  program("1 + 2;  if true {3} else {4};  let x = 1; x + 1") should parseTo[Program](
    Program(Vector(PlusExp(IntExp(1), IntExp(2)),
                   IfExp(BoolExp(true), Block(Vector(IntExp(3))), Block(Vector(IntExp(4)))),
                   LetDecl(IdnDef("x"), IntExp(1)),
                   PlusExp(IdnExp(IdnUse("x")), IntExp(1)))))
  }

  test("programs with comments in them work") {
    program("""
            |42; // On a line with code
            |// On a line by itself
            |
            |1 + // In an expression
            |99
            |
            |     // Over multiple
            | // lines with some code in it:
            | // 4 + 3;
            |""".stripMargin) should parseTo[Program](
      Program(Vector(IntExp(42), PlusExp(IntExp(1), IntExp(99)))))
  }

  test("factorial program produces the correct tree (file src/test/resources/factorial.lin)") {

    program("""
            |fn fact(n: int) -> int {
            |  if n = 0 { 1 } else { n * fact(n-1) }
            |};
            |
            |fact(5)""".stripMargin) should parseTo[Program](
      Program(
        Vector(
          FnDecl(IdnDef("fact"),
                 Vector(ParamDecl(IdnDef("n"),IntType())),
                 Some(IntType()),
                 Block(
                   Vector(
                     IfExp(
                       EqualExp(IdnExp(IdnUse("n")), IntExp(0)),
                       Block(Vector(IntExp(1))),
                       Block(
                         Vector(
                           StarExp(
                             IdnExp(IdnUse("n")),
                             AppExp(IdnExp(IdnUse("fact")),
                                    Vector(MinusExp(IdnExp(IdnUse("n")),
                                                    IntExp(1))))))))))),
          AppExp(IdnExp(IdnUse("fact")),Vector(IntExp(5))))))
  }

  test("miscellaneous test expressions produce the correct trees (file src/text/resources/tests.lin)") {
    program("""
            |42;
            |
            |3 + 4;
            |
            |2 * (12 / 4);
            |
            |if x < 0 { 1 } else { 2 };
            |
            |if x = y {
            |  if true { 3 } else { 4 }
            |} else {
            |  5
            |};
            |
            |1 + (if x < 10 { 10 } else { x });
            |
            |{
            |  fn double(x : int) -> int {
            |    x * 2
            |  };
            |  double(4)
            |};
            |
            |{ let x = 5; x + x } * { let y = 10; y / 4 };
            |
            |fn uncurried(x : int, y : bool) -> int {
            |  if y { x } else { x * 2 }
            |};
            |
            |fn curried (x : int) -> fn(bool) -> int {
            |  fn curried1(y : bool) -> int {
            |    if y { x } else { x * 2 }
            |  };
            |  curried1
            |};
            |
            |fn curry(f : fn (int, bool) -> int) -> (fn(int) -> (fn(bool) -> int)) {
            |  fn curry1 (x : int) -> fn(bool) -> int {
            |    fn curry2 (y : bool) -> int {
            |      f(x,y)
            |    };
            |    curry2
            |  };
            |  curry1
            |};
            |
            |fn uncurry(f : fn(int) -> (fn(bool) -> int)) -> (fn(int, bool) -> int) {
            |  fn uncurry1(x : int, y : bool) -> int {
            |    f(x)(y) 
            |  };
            |  uncurry1
            |}
            |""".stripMargin) should parseTo[Program](
      Program(
        Vector(
          IntExp(42),
          PlusExp(IntExp(3), IntExp(4)),
          StarExp(IntExp(2), SlashExp(IntExp(12), IntExp(4))),
          IfExp(
            LessExp(IdnExp(IdnUse("x")), IntExp(0)),
            Block(Vector(IntExp(1))),
            Block(Vector(IntExp(2)))),
          IfExp(
            EqualExp(IdnExp(IdnUse("x")), IdnExp(IdnUse("y"))),
            Block(
              Vector(
                IfExp(
                  BoolExp(true),
                  Block(Vector(IntExp(3))),
                  Block(Vector(IntExp(4)))))),
            Block(Vector(IntExp(5)))),
          PlusExp(
            IntExp(1),
            IfExp(
              LessExp(IdnExp(IdnUse("x")), IntExp(10)),
              Block(Vector(IntExp(10))),
              Block(Vector(IdnExp(IdnUse("x")))))),
          Block(
            Vector(
              FnDecl(
                IdnDef("double"),
                Vector(ParamDecl(IdnDef("x"), IntType())),
                Some(IntType()),
                Block(Vector(StarExp(IdnExp(IdnUse("x")), IntExp(2))))),
              AppExp(IdnExp(IdnUse("double")), Vector(IntExp(4))))),
          StarExp(
            Block(
              Vector(
                LetDecl(IdnDef("x"), IntExp(5)),
                PlusExp(IdnExp(IdnUse("x")), IdnExp(IdnUse("x"))))),
            Block(
              Vector(
                LetDecl(IdnDef("y"), IntExp(10)),
                SlashExp(IdnExp(IdnUse("y")), IntExp(4))))),
          FnDecl(
            IdnDef("uncurried"),
            Vector(
              ParamDecl(IdnDef("x"), IntType()),
              ParamDecl(IdnDef("y"), BoolType())),
            Some(IntType()),
            Block(
              Vector(
                IfExp(
                  IdnExp(IdnUse("y")),
                  Block(Vector(IdnExp(IdnUse("x")))),
                  Block(
                    Vector(StarExp(IdnExp(IdnUse("x")), IntExp(2)))))))),
          FnDecl(
            IdnDef("curried"),
            Vector(ParamDecl(IdnDef("x"), IntType())),
            Some(FnType(Vector(BoolType()), IntType())),
            Block(
              Vector(
                FnDecl(
                  IdnDef("curried1"),
                  Vector(ParamDecl(IdnDef("y"), BoolType())),
                  Some(IntType()),
                  Block(
                    Vector(
                      IfExp(
                        IdnExp(IdnUse("y")),
                        Block(Vector(IdnExp(IdnUse("x")))),
                        Block(
                          Vector(
                            StarExp(
                              IdnExp(IdnUse("x")),
                              IntExp(2)))))))),
                IdnExp(IdnUse("curried1"))))),
          FnDecl(
            IdnDef("curry"),
            Vector(
              ParamDecl(
                IdnDef("f"),
                FnType(Vector(IntType(), BoolType()), IntType()))),
            Some(
              FnType(
                Vector(IntType()),
                FnType(Vector(BoolType()), IntType()))),
            Block(
              Vector(
                FnDecl(
                  IdnDef("curry1"),
                  Vector(ParamDecl(IdnDef("x"), IntType())),
                  Some(FnType(Vector(BoolType()), IntType())),
                  Block(
                    Vector(
                      FnDecl(
                        IdnDef("curry2"),
                        Vector(
                          ParamDecl(IdnDef("y"), BoolType())),
                        Some(IntType()),
                        Block(
                          Vector(
                            AppExp(
                              IdnExp(IdnUse("f")),
                              Vector(
                                IdnExp(IdnUse("x")),
                                IdnExp(IdnUse("y"))))))),
                      IdnExp(IdnUse("curry2"))))),
                IdnExp(IdnUse("curry1"))))),
          FnDecl(
            IdnDef("uncurry"),
            Vector(
              ParamDecl(
                IdnDef("f"),
                FnType(
                  Vector(IntType()),
                  FnType(Vector(BoolType()), IntType())))),
            Some(FnType(Vector(IntType(), BoolType()), IntType())),
            Block(
              Vector(
                FnDecl(
                  IdnDef("uncurry1"),
                  Vector(
                    ParamDecl(IdnDef("x"), IntType()),
                    ParamDecl(IdnDef("y"), BoolType())),
                  Some(IntType()),
                  Block(
                    Vector(
                      AppExp(
                        AppExp(
                          IdnExp(IdnUse("f")),
                          Vector(IdnExp(IdnUse("x")))),
                        Vector(IdnExp(IdnUse("y"))))))),
                IdnExp(IdnUse("uncurry1"))))))))
  }
}

