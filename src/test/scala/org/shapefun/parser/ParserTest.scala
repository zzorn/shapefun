package org.shapefun.parser

import func.{ParameterInfo, SimpleFunc, Func, MathFunctions}
import org.scalatest.FunSuite
import org.scalatest.Assertions._
import org.parboiled.errors.ParsingException
import syntaxtree.{Num, Expr}
import org.shapefun.utils.StepRange

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

    shouldNotParse("1 == 1 == 1")
    shouldNotParse("1 != 1 == 1")
    shouldNotParse("1 == 1 != 1")
    shouldNotParse("1 != 1 != 1")

    shouldParseToBool("1 < 2 < 3", true)
    shouldParseToBool("1 < 2 > 1", true)
    shouldParseToBool("1 <= 2 >= 2", true)
    shouldParseToBool("10 > 4 >= -1", true)
    shouldParseToBool("10 > 4 >= 5", false)
    shouldParseToBool("10 > -4 >= -1", false)
    shouldParseToBool("10 > 11 >= -1", false)
  }

  test("Equals and not equals") {
    shouldParseToBool("1 == 1", true)
    shouldParseToBool("-1 == 0", false)
    shouldParseToBool("1 == true", false)
    shouldParseToBool("false == true", false)
    shouldParseToBool("true == true", true)
    shouldParseToBool("false == false", true)

    shouldParseToBool("false != false", false)
    shouldParseToBool("false != true", true)
    shouldParseToBool("false != 1", true)
    shouldParseToBool("1 != 1", false)
  }

  test("Boolean expressions") {
    shouldParseToBool("true", true)
    shouldParseToBool("false", false)
    shouldParseToBool("true or false", true)
    shouldParseToBool("false or false", false)
    shouldParseToBool("false and true", false)
    shouldParseToBool("true and true", true)
    shouldParseToBool("true xor true", false)
    shouldParseToBool("false xor true", true)
    shouldParseToBool("false xor false", false)

    shouldParseToBool("false or false or true", true)
    shouldParseToBool("false or false or false", false)
    shouldParseToBool("false and true and false", false)
    shouldParseToBool("true and true and true", true)

    shouldParseToBool("not true", false)
    shouldParseToBool("not false", true)
    shouldParseToBool("not false or true", true)

    shouldParseToBool("(1 > 0) and true", true)
    shouldParseToBool("1 > 0 and true", true)
    shouldParseToBool("1 > 0 and false", false)
    shouldParseToBool("1 > 0 or false", true)
    shouldParseToBool("not 1 > 0 or false", false)
    shouldParseToBool("(true or false) xor false", true)
    shouldParseToBool("(true or false) xor 0 < 1", false)
    shouldParseToBool("true and true and true and true", true)
    shouldParseToBool("true and false or true and false", false)
  }


  test("If") {
    shouldParseTo("if 1 > 0 then 5 else 6", 5)
    shouldParseTo("if 1 < 0 then 5 else 6", 6)
  }

  test("Block") {
    shouldParseTo("3 + \n 3", 6)
    shouldParseTo("3 \n + 3", 6)
    shouldParseTo("3", 3)
    shouldParseTo("3;", 3)
    shouldParseTo(" 3 ; ", 3)
    shouldParseTo("1; 2; 3", 3)
    shouldParseTo("1\n 2\n 3", 3)
    shouldNotParse("1 2 3")
  }

  /* For loop alternatives
  - Can a for loop be an expression?  What to return?
   - Nothing intuitive -> return nothing.  To make it an expression, maybe return number of times looped?
   //- Or maybe support yield, and return list with all yielded values?

  for x in 10..-10 step 2,
      y in rows,
      z in [1, -1] do {
    setPixel(x, y, x*y*z)
    doSth( rel(x) )

    // Relative position along loop range, always 0 = first element, 1 = last element, interpolate in between, if only 1 element, 0
    relative(x) // Probably cleanest, similar syntax to function call
    rel(x)  // shorter, more practical to use
    ~x // very concise, but somewhat cryptic, and uses an operator char
    x~
    x% // more intuitive.  Some potential confusion with modulo operator, which might be good to have too
    %x

  }

  // for x = 1; x < 10; x++ do {}  // This is somewhat redundant, not so intuitive.  for x in -syntax is more useful for majority of cases.

  var x = 1
  while x < 10 do { x++}


  Ranges:

    0..10     (both inclusive, or end exclusive?)
    0..100 step 5     // In 20 steps of 5
    0..100 steps 5    // In 5 steps of 20
    0..100 in 5 steps // In 5 steps of 20 - clearer syntax, but more verbose and not following same pattern


   */

  test("Variable declaration and assignment") {
    shouldParseTo(
      """
        | var a = 5 + 2
        | var b = a * 4 - 1
        | b = b + 1
        | a + b
      """.stripMargin, 35)

    shouldParseTo(
      """
        | var foo = 5
        | foo += 5
        | foo *= 2
        | foo /= 3+2
        | foo
      """.stripMargin, 4)

    shouldNotCalculate("foo = 3") // Need to declare before use
    shouldNotParse("var bar") // Need to provide initial value when declaring
  }

  test("Inc and dec ops") {
    shouldParseTo(
      """
        | var a = 1
        | a++
        | a++
        | a
      """.stripMargin, 3)

    shouldParseTo(
      """
        | var a = 1
        | a++
        | a++
        | a++
      """.stripMargin, 3)

    shouldParseTo(
      """
        | var a = 1
        | a++
        | a--
        | a++
        | a--
      """.stripMargin, 2)

    shouldParseTo(
      """
        | var foo = 1
        | 4* -foo++ + 3
      """.stripMargin, -1)

    shouldNotParse("1++") // Increment operator only works on variables
    shouldNotParse("var a; ++a") // Prefix increment and decrement not implemented
  }

  test("Range") {
    shouldParseToObj("10..100", StepRange(10, 100))
    shouldParseToObj(" 10 .. 100 ", StepRange(10, 100))
    shouldParseToObj(" 10.. 100 ", StepRange(10, 100))
    shouldParseToObj(" 10 ..100 ", StepRange(10, 100))
    shouldParseToObj("10...100", StepRange(10, 100, 1, true))
    shouldParseToObj("10 ...100 step 2", StepRange(10, 100, 2, true))
    shouldParseToObj("0... 100 steps 5", StepRange.withSteps(0, 100, 5, true))

    shouldParseToObj("10..-10", StepRange(10, -10))
    shouldParseToObj("10 ... -10", StepRange(10, -10, 1, true))
    shouldParseToObj("10..-10 step 3", StepRange(10, -10, 3, false))

    shouldParseToObj("0..1 steps 10", StepRange(0, 1, 0.1))
    shouldParseToObj("0.5..0.7 step 0.1", StepRange(0.5, 0.7, 0.1))

    shouldParseToObj("3+2 ..-3 -1 step 10/2", StepRange(5, -4, 5))
    shouldParseToBool("3+2 .. -3 -1 step 10/2 == 5+2-2 .. 8-12 step 5", true)
  }



  // TODO: Function definition

  // TODO: For
  // TODO: While

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


