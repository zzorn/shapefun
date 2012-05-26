package org.shapefun.mathfuncs

import math._
import org.shapefun.parser.syntaxtree.Num
import org.shapefun.parser.defs.{FunDef, ParamInfo, ExternalFunDef}
import org.shapefun.parser.{Context, ContextImpl}
import java.util.Random

/**
 * Common math functions
 */
object MathContext {

  private val _context = ContextImpl()

  def context: Context = _context

  _context.addVal('Pi, Double.box(Pi))
  _context.addVal('Tau, Double.box(2 * Pi))
  _context.addVal('E, Double.box(E))

  _context.addVar('randomNumberGenerator, new Random())

  addFunc1('abs) { abs(_) }
  addFunc1('signum) { signum(_) }
  addFunc1('ceil) { ceil(_) }
  addFunc1('floor) { floor(_) }
  addFunc1('round) { round(_) }

  addFunc1('toDegrees) { toDegrees(_) }
  addFunc1('toRadians) { toRadians(_) }

  addFunc2('max) { max(_, _) }
  addFunc2('min) { min(_, _) }

  addFunc2('pow) { pow(_, _) }
  addFunc1('sqrt) { sqrt(_) }

  addFunc1('log) { log(_) }
  addFunc1('log10) { log10(_) }
  addFunc1('exp) { exp(_) }

  addFunc1('sin)  { sin(_)  }
  addFunc1('cos)  { cos(_)  }
  addFunc1('tan)  { tan(_)  }
  addFunc1('asin) { asin(_) }
  addFunc1('acos) { acos(_) }
  addFunc1('atan) { atan(_) }

  addFunc0('random) { _context.getValue('randomNumberGenerator).asInstanceOf[Random].nextDouble() }





  private def addFunc0(name: Symbol)
                     (f: => Double) {
    _context.addFun(createMathFunc0(name, f))
  }

  private def addFunc1(name: Symbol,
                      p1: Symbol = 'a)
                     (f: Double => Double) {
    _context.addFun(createMathFunc1(name, p1, f))
  }

  private def addFunc2(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b)
                     (f: (Double, Double) => Double) {
    _context.addFun(createMathFunc2(name, p1, p2, f))
  }

  private def addFunc3(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b,
                      p3: Symbol = 'c)
                     (f: (Double, Double, Double) => Double) {
    _context.addFun(createMathFunc3(name, p1, p2, p3, f))
  }

  private def addFunc4(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b,
                      p3: Symbol = 'c,
                      p4: Symbol = 'd)
                     (f: (Double, Double, Double, Double) => Double) {
    _context.addFun(createMathFunc4(name, p1, p2, p3, p4, f))
  }

  private def addFunc5(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b,
                      p3: Symbol = 'c,
                      p4: Symbol = 'd,
                      p5: Symbol = 'e)
                     (f: (Double, Double, Double, Double, Double) => Double) {
    _context.addFun(createMathFunc5(name, p1, p2, p3, p4, p5, f))
  }

  private def addFunc6(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b,
                      p3: Symbol = 'c,
                      p4: Symbol = 'd,
                      p5: Symbol = 'e,
                      p6: Symbol = 'f)
                     (f: (Double, Double, Double, Double, Double, Double) => Double) {
    _context.addFun(createMathFunc6(name, p1, p2, p3, p4, p5, p6, f))
  }

  private def addFunc7(name: Symbol,
                      p1: Symbol = 'a,
                      p2: Symbol = 'b,
                      p3: Symbol = 'c,
                      p4: Symbol = 'd,
                      p5: Symbol = 'e,
                      p6: Symbol = 'f,
                      p7: Symbol = 'g)
                     (f: (Double, Double, Double, Double, Double, Double, Double) => Double) {
    _context.addFun(createMathFunc7(name, p1, p2, p3, p4, p5, p6, p7, f))
  }




  def createMathFunc0(name: Symbol,
                     mathFunc: => Double): FunDef =
    ExternalFunDef(name, List(), Num.Class, {
      args =>
        Double.box(mathFunc)
    })

  def createMathFunc1(name: Symbol,
                     param1Name: Symbol,
                     mathFunc: (Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1Name, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1Name))
        ))
    })

  def createMathFunc2(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     mathFunc: (Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2))
        ))
    })

  def createMathFunc3(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     param3: Symbol,
                     mathFunc: (Double, Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class),
      ParamInfo(param3, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2)),
          Double.unbox(args(param3))
        ))
    })

  def createMathFunc4(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     param3: Symbol,
                     param4: Symbol,
                     mathFunc: (Double, Double, Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class),
      ParamInfo(param3, Num.Class),
      ParamInfo(param4, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2)),
          Double.unbox(args(param3)),
          Double.unbox(args(param4))
        ))
    })

  def createMathFunc5(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     param3: Symbol,
                     param4: Symbol,
                     param5: Symbol,
                     mathFunc: (Double, Double, Double, Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class),
      ParamInfo(param3, Num.Class),
      ParamInfo(param4, Num.Class),
      ParamInfo(param5, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2)),
          Double.unbox(args(param3)),
          Double.unbox(args(param4)),
          Double.unbox(args(param5))
        ))
    })

  def createMathFunc6(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     param3: Symbol,
                     param4: Symbol,
                     param5: Symbol,
                     param6: Symbol,
                     mathFunc: (Double, Double, Double, Double, Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class),
      ParamInfo(param3, Num.Class),
      ParamInfo(param4, Num.Class),
      ParamInfo(param5, Num.Class),
      ParamInfo(param6, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2)),
          Double.unbox(args(param3)),
          Double.unbox(args(param4)),
          Double.unbox(args(param5)),
          Double.unbox(args(param6))
        ))
    })

  def createMathFunc7(name: Symbol,
                     param1: Symbol,
                     param2: Symbol,
                     param3: Symbol,
                     param4: Symbol,
                     param5: Symbol,
                     param6: Symbol,
                     param7: Symbol,
                     mathFunc: (Double, Double, Double, Double, Double, Double, Double) => Double): FunDef =
    ExternalFunDef(name, List(
      ParamInfo(param1, Num.Class),
      ParamInfo(param2, Num.Class),
      ParamInfo(param3, Num.Class),
      ParamInfo(param4, Num.Class),
      ParamInfo(param5, Num.Class),
      ParamInfo(param6, Num.Class),
      ParamInfo(param7, Num.Class)
    ), Num.Class, {
      args =>
        Double.box(mathFunc(
          Double.unbox(args(param1)),
          Double.unbox(args(param2)),
          Double.unbox(args(param3)),
          Double.unbox(args(param4)),
          Double.unbox(args(param5)),
          Double.unbox(args(param6)),
          Double.unbox(args(param7))
        ))
    })

}