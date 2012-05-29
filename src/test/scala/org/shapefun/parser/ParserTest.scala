package org.shapefun.parser

import defs.{ParamInfo, ExternalFunDef}
import org.shapefun.mathfuncs.MathContext
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
    val context1 = ContextImpl()
    context1.addVal('a, 5)
    shouldParseTo("a", 5, context1)

    val context2 = ContextImpl()
    context2.addVal('foo, 2.0)
    context2.addVal('bar, 3.0)
    shouldParseTo("-foo + bar * 2", 4, context2)
    shouldNotCalculate("foobar")
  }

  test("External function") {

    shouldParseTo("abs(-1)", 1, MathContext.context)
    shouldParseTo(" - pow( 2.0, 3.0 ) * abs( -2)", -16, MathContext.context)
  }

  test("Calling function on object") {
    val context = ContextImpl()
    val fooKind: JavaKind = JavaKind(classOf[Foo])
    context.addFun(new ExternalFunDef('createFoo, List(ParamInfo('a, Num.Kind)), fooKind, {params =>
      Foo("bar", params('a).asInstanceOf[Num.NumType])
    }))
    context.addExtFun(classOf[Foo], new ExternalFunDef('invokeBaz, List(
      ParamInfo('self, fooKind),
      ParamInfo('a, Num.Kind)
    ), Num.Kind, {params =>
      Double.box(params('self).asInstanceOf[Foo].zap + params('a).asInstanceOf[Num.NumType])
    }))

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


  test("For loops") {
    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10 do foo += 2
        | foo
      """.stripMargin, 20)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..0 do foo += 2
        | foo
      """.stripMargin, 0)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0...0 do foo += 2
        | foo
      """.stripMargin, 2)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10,
        |     y in 0..10 do foo += 2
        | foo
      """.stripMargin, 200)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10,
        |     y in 0..10,
        |     z in 0..10 do foo += 2
        | foo
      """.stripMargin, 2000)

    shouldParseTo(
      """
        | var foo = 0
        | var bar = 1
        | for x in 0..10 do {
        |   foo += 2
        |   bar += 3
        | }
        | foo + bar
      """.stripMargin, 51)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10 step 2 do foo++
        | foo
      """.stripMargin, 5)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0...10 steps 5 do foo++
        | foo
      """.stripMargin, 5)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10 steps 5 do foo++
        | foo
      """.stripMargin, 5)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..10 steps 3 do foo++
        | foo
      """.stripMargin, 3)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0..1 step 0.1 do foo++
        | foo
      """.stripMargin, 10)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0...10 do if x==2 then foo = x_place
        | foo
      """.stripMargin, 0.2)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0...10 do if x==0 then foo = x_index
        | foo
      """.stripMargin, 0)

    shouldParseTo(
      """
        | var foo = 0
        | for x in -10...10 do if x==-5 then foo = x_index
        | foo
      """.stripMargin, 5)

    shouldParseTo(
      """
        | var foo = 0
        | for x in 0...2, y in 0...2 do foo += x*y
        | foo
      """.stripMargin, 9)

    shouldParseTo(
      """
        | var foo = 0
        | var x = 4
        | for x in 0..10 do foo += x
        | x
      """.stripMargin, 4)

    shouldNotCalculate(
      """
        | var foo = 0
        | for x in 0..10 do foo += x
        | x
      """.stripMargin)
    shouldNotParse(
      """
        | var foo = 0
        | for x in 0..10 do
      """.stripMargin)
  }

  test("Complicated loop") {
    shouldParseToBool(
      """
        | var start = 100
        | var foo = 0
        | for x in start...-12.5 steps 2,
        |     y in -30+4...222-start step 0.1,
        |     z in 1..1 do {
        |   foo += x*  y_place * z_index
        |   foo += x / y_index + z_place + z_index + z
        | }
        | foo != 0
      """.stripMargin, true)
  }

  // NOTE: if type is omitted, assume double?  Get return type from last expression in block if not defined
  test("Function definition with type defs") {
    shouldParseTo(
      """
        | def foo(a: Num, b: Num): Num {
        |   a + b
        | }
        |
        | foo(1, 2)
      """.stripMargin, 3)
  }

  test("Function calling with named arguments") {
    shouldParseTo(
      """
        | def foo(a: Num, b: Num, c: Num): Num {
        |   a + b - c
        | }
        |
        | foo(1, c=2, b=4)
      """.stripMargin, 3)

    shouldParseTo(
      """
        | def foo(a: Num, b: Num = 4, c: Num = 2): Num {
        |   a + b - c
        | }
        |
        | foo(1, c=2)
      """.stripMargin, 3)

    shouldParseTo(
      """
        | def foo(a: Num= 1, b: Num = 2, c: Num = 4): Num {
        |   a + b - c
        | }
        |
        | foo(100, c=200, 400)
      """.stripMargin, 300)

  }

  test("Function definition should have own scope") {
    shouldParseTo(
      """
        | var a = 10
        | def foo(a: Num, b: Num): Num {
        |   var c = b
        |   a + c
        | }
        |
        | a + foo(1, 2)
      """.stripMargin, 13)
  }

  test("Function definition with default parameters") {
    shouldParseTo(
      """
        | def foo(a: Num, b: Num = 4): Num {
        |   a + b
        | }
        |
        | foo(1)
      """.stripMargin, 5)

    shouldParseTo(
      """
        | def foo(a: Num, b = 4): Num {
        |   a + b
        | }
        |
        | foo(1)
      """.stripMargin, 5)
  }

  test("Function definition with omitted types") {
    shouldParseTo(
      """
        | def foo(a, b) {
        |   a + b
        | }
        |
        | foo(1, 2)
      """.stripMargin, 3)

    shouldParseTo(
      """
        | def foo(a, b = 4) {
        |   a + b
        | }
        |
        | foo(1)
      """.stripMargin, 5)
  }

  test("Function definition with one expression") {
    shouldParseTo(
      """
        | def foo(a, b) = a + b
        |
        | foo(1, 2)
      """.stripMargin, 3)

    shouldParseTo(
      """
        | def foo(a, b = 4) = a + b
        |
        | foo(1)
      """.stripMargin, 5)
  }

  test("Nested function definitions") {
    shouldParseTo(
      """
        | var d = 1
        | def foo(a, b, c) {
        |   def bar(x, b) {
        |     x + b
        |   }
        |
        |   bar(a, b) + c + d
        | }
        |
        | foo(2, 4, 8)
      """.stripMargin, 15)

    shouldNotCalculate(
      """
        | var d = 1
        | def foo(a, b, c) {
        |   def bar(x, b) {
        |     x + b
        |   }
        |
        |   bar(a, b) + c + d
        | }
        |
        | bar(2, 4)
      """.stripMargin)

    shouldNotCalculate(
      """
        | var d = 1
        | def foo(a, b, c) {
        |   def bar(x, b) {
        |     x + b
        |   }
        |
        |   bar(a, b) + x
        | }
        |
        | foo(2, 4, 8)
      """.stripMargin)
  }

  // TODO: 3D model generation, with some utility methods

  // TODO: Flesh out math lib, e.g. lerps, tweening, random.

  // TODO: While

  // TODO: Function parameters, closures?

  // TODO: List, Map, Set? syntaxes

  // TODO: Loop through collections with for loops

  // TODO: Imports

  // TODO: Deduct type from values if type info is omitted

  // TODO: Test for not allowing too many parameters

  // TODO: Test for type checking

  // TODO: Enum types? / static object?

  // TODO: Extract language to own project



  def shouldParseTo(expression: String, expected: Double, context: Context = ContextImpl(), definedKinds: Map[Symbol, Kind] = Map()) {
    val parser = new ShapeLangParser(definedKinds)
    val expr: Expr = parser.parse(expression, context)
    val result: Any = expr.calculate(context)


    // If both values are NaN, test should also pass. Otherwise, do assert.
    if (!(result.asInstanceOf[Double].isNaN && expected.isNaN)) {

      // Print some debugging help on fail
      if (result != expected) println("Expression was: " + expr)

      assert(result === expected)
    }
  }

  def shouldParseToBool(expression: String, expected: Boolean, context: Context = ContextImpl(), definedKinds: Map[Symbol, Kind] = Map()) {
    shouldParseToObj(expression, Boolean.box(expected), context, definedKinds)
  }

  def shouldParseToObj(expression: String, expected: AnyRef, context: Context = ContextImpl(), definedKinds: Map[Symbol, Kind] = Map()) {
    val parser = new ShapeLangParser(definedKinds)
    val expr: Expr = parser.parse(expression, context)
    val result: Any = expr.calculate(context)

    // Print some debugging help on fail
    if (result != expected) println("Expression was: " + expr)

    assert(result === expected)
  }

  def shouldNotParse(expression: String, definedKinds: Map[Symbol, Kind] = Map()) {
    val parser = new ShapeLangParser(definedKinds)
    intercept[ParsingException](parser.parse(expression, new ContextImpl()))
  }

  def shouldNotCalculate(expression: String, context: Context = ContextImpl(), definedKinds: Map[Symbol, Kind] = Map()) {
    val parser = new ShapeLangParser(definedKinds)
    val expr: Expr = parser.parse(expression, context)
    intercept[Exception](expr.calculate(context))
  }


  case class Foo(bar: String, zap: Double) {

  }
}


