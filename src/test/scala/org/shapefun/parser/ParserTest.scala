package org.shapefun.parser

import func.{ParameterInfo, SimpleFunc, Func, MathFunctions}
import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import syntaxtree.{Num, Expr}

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
    shouldParseTo("((1))", 1)
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

  test("External variable") {
    shouldParseTo("a", 5, SimpleContext(Map('a -> Double.box(5.0))))
    shouldParseTo("-foo + bar * 2", 4, SimpleContext(Map('foo -> Double.box(2.0), 'bar -> Double.box(3.0))))
    shouldNotCalculate("foobar")
  }

  test("External function") {
    shouldParseTo("abs(-1)", 1, SimpleContext(functions = MathFunctions.functions))
    shouldParseTo(" - pow( 2.0, 3.0 ) * abs( -2)", -16, SimpleContext(functions = MathFunctions.functions))
  }

  test("Calling function on object") {
    val context: SimpleContext = SimpleContext(Map(),
      List(SimpleFunc('createFoo,
        List(ParameterInfo('a, Num.Class)),
        classOf[Foo],
        {params =>
          Foo("bar", params(0).asInstanceOf[Num.NumType])
        })),

      List(SimpleClassInfo(classOf[Foo],
        List(SimpleFunc('invokeBaz, List(ParameterInfo('a, Num.Class)),
        Num.Class,
        {params =>
          Double.box(params(0).asInstanceOf[Foo].zap + params(1).asInstanceOf[Num.NumType])
        }
        ))))
    )

    shouldParseToObj("createFoo(3)", Foo("bar", 3), context)
    shouldParseTo("createFoo(4).invokeBaz(3)", 7, context)
    shouldParseTo("-createFoo(4).invokeBaz(3)", -7, context)
    shouldParseTo("(createFoo(4)).invokeBaz(3)", 7, context)
    shouldParseTo("2 * createFoo(4).invokeBaz(3)", 14, context)
  }

  test("Comparision expressions") {
    shouldParseToBool("1 < 0", false)
    shouldParseToBool("1 > 0", true)
    shouldParseToBool("1 > 1", false)
    shouldParseToBool("1 >= 1", true)
    shouldParseToBool("1 <= 1", true)
    shouldParseToBool("1 == 1", true)
    shouldParseToBool("1 != 1", false)
    shouldParseToBool("1 > 1", false)
    shouldParseToBool("1 < 1", false)

    shouldParseToBool("1 < 2 < 3", true)
    shouldParseToBool("1 < 2 > 1", true)
    shouldParseToBool("1 <= 2 >= 2", true)
    shouldParseToBool("10 > 4 >= -1", true)
    shouldParseToBool("10 > 4 >= 5", false)
    shouldParseToBool("10 > -4 >= -1", false)
    shouldParseToBool("10 > 11 >= -1", false)
  }

  test("Boolean expressions") {
    shouldParseToBool("1 > 0 and true", true)
    shouldParseToBool("1 > 0 and false", false)
    shouldParseToBool("1 > 0 or false", true)
    shouldParseToBool("not 1 > 0 or false", false)
    shouldParseToBool("(true or false) xor false", true)
    shouldParseToBool("(true or false) xor 0 < 1", false)
    shouldParseToBool("true and true and true and true", true)
    shouldParseToBool("true and false or true and false", false)
  }

  // TODO: Boolean expressions (and, or, not etc)
  // TODO: Number comparison (<, >, ==, <> etc)

  // TODO: Function definition

  // TODO: If
  // TODO: For
  // TODO: While
  // TODO: Switch

  // TODO: Variable and value definitions, variable update

  // TODO: List, Map, Set? syntaxes

  // TODO: Instead of class instantiation, call externally defined functions that create the necessary class (e.g. model, color, etc)

  // TODO: Function parameters, closures?

  // TODO: Enum types? / static object?

  // TODO: Imports


  // TODO: Test for not allowing too many parameters

  // TODO: Test for type checking

  // TODO: Extract language to own project


  def shouldParseTo(expression: String, expected: Double, context: Context = SimpleContext()) {
    val parser = new ShapeLangParser()
    val expr: Expr = parser.parse(expression)
    val result: Any = expr.calculate(context)


    // If both values are NaN, test should also pass. Otherwise, do assert.
    if (!(result.asInstanceOf[Double].isNaN && expected.isNaN)) {

      // Print some debugging help on fail
      if (result != expected) println("Expression was: " + expr)

      assert(result === expected)
    }
  }

  def shouldParseToBool(expression: String, expected: Boolean, context: Context = SimpleContext()) {
    shouldParseToObj(expression, Boolean.box(expected), context)
  }

  def shouldParseToObj(expression: String, expected: AnyRef, context: Context = SimpleContext()) {
    val parser = new ShapeLangParser()
    val expr: Expr = parser.parse(expression)
    val result: Any = expr.calculate(context)

    // Print some debugging help on fail
    if (result != expected) println("Expression was: " + expr)

    assert(result === expected)
  }

  def shouldNotParse(expression: String) {
    val parser = new ShapeLangParser()
    intercept[ParsingException](parser.parse(expression))
  }

  def shouldNotCalculate(expression: String, context: Context = SimpleContext()) {
    val parser = new ShapeLangParser()
    val expr: Expr = parser.parse(expression)
    intercept[Exception](expr.calculate(context))
  }


  case class Foo(bar: String, zap: Double) {

  }
}


