package org.shapefun.parser

import org.scalatest.FunSuite
import syntaxtree.Expr
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException

/**
 *
 */
class ParserTest extends FunSuite {

  test("Parse plain numbers") {
    shouldParseTo("0", 0)
    shouldParseTo("1", 1)
    shouldParseTo("123456983", 123456983)
  }

  test("Parse negative numbers") {
    shouldParseTo("-1", -1)
    shouldParseTo("-999", -999)
  }

  test("Numbers should not have leading zero") {
    shouldParseTo("0", 0)
    shouldParseTo("-0", 0)
    shouldNotParse("00")
    shouldNotParse("-000")
    shouldNotParse("-023")
    shouldNotParse("01")
    shouldNotParse("01.0")
    shouldNotParse("00.0")
    shouldParseTo("0.0", 0)
  }

  test("Parse float numbers") {
    shouldParseTo("1.0", 1.0)
    shouldParseTo("0.5", 0.5)
    shouldParseTo("-0.1", -0.1)
    shouldParseTo("3.1415", 3.1415)
    shouldParseTo("9999.99", 9999.99)
    shouldParseTo("-9999.99", -9999.99)
  }

  test("Whitespace should be stripped") {
    shouldParseTo("1.0", 1.0)
    shouldParseTo("1.0  ", 1.0)
    shouldParseTo("  1.0", 1.0)
    shouldParseTo("  1.0  ", 1.0)
  }

  test("Calculation") {
    shouldParseTo("1+1", 2)
    shouldParseTo("1 + 2", 3)
    shouldParseTo("1+2+3", 6)

    shouldParseTo("1+2*3", 7)
    shouldParseTo("1*2+3", 5)
    shouldParseTo("2 * 3 * 4", 24)

    shouldParseTo("2-1", 1)
    shouldParseTo("2-3", -1)
    shouldParseTo("2-3+4", 3)
    shouldParseTo(" 2 + 3 - 4 ", 1)
    shouldParseTo("-2 - 3- 4", -9)
    shouldParseTo("2 - -3", 5)
    shouldParseTo("2 --3", 5)
    shouldParseTo("2--3", 5)

    shouldParseTo("10/2", 5)
    shouldParseTo("10/2*3", 15)
    shouldParseTo("0/0", Double.NaN)
    shouldParseTo("1/0", Double.PositiveInfinity)
    shouldParseTo("-1/0", Double.NegativeInfinity)
  }

  test("Parenthesis") {
    shouldParseTo("(1)", 1)
    shouldParseTo(" ( 1 ) ", 1)
    shouldParseTo("2-(3)", -1)
    shouldParseTo("(2)-(3)", -1)
    shouldParseTo("(2)-3", -1)
    shouldParseTo("10/2*3", 15.0)
    shouldParseTo("20/(2*3)", 20.0/6)
    shouldParseTo("10 / ( 2 * 3 ) ", 10.0/6)
    shouldParseTo("10 /( 2*3) ", 10.0/6)
  }


  test("Unary minus") {
    shouldParseTo("-(2+3)", -5)
    shouldParseTo("- (3) - (2)", -5)
  }

  def shouldParseTo(expression: String, expected: Double) {
    val parser = new ShapeLangParser()
    val expr: Expr = parser.parse(expression)
    val result: Any = expr.calculate(null)


    // If both values are NaN, test should also pass. Otherwise, do assert.
    if (!(result.asInstanceOf[Double].isNaN && expected.isNaN)) {

      // Print some debugging help on fail
      if (result != expected) println("Expression was: " + expr)

      assert(result === expected)
    }
  }

  def shouldNotParse(expression: String) {
    val parser = new ShapeLangParser()
    intercept[ParsingException](parser.parse(expression))
  }
}