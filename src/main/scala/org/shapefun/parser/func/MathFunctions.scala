package org.shapefun.parser.func

import math._

/**
 * Common math functions
 */
object MathFunctions {

  private var _functions = Map[Symbol, Func]()

  def functions: Map[Symbol, Func] = _functions


  addFunc1('abs) { abs(_) }

  addFunc2('pow) { pow(_, _) }

  addFunc1('sin) { sin(_) }
  addFunc1('cos) { cos(_) }
  addFunc1('tan) { tan(_) }


  private def addFunc1(name: Symbol, p1: Symbol = 'a)(f: Double => Double) {
    _functions += name -> MathFunc1(name, p1, f)
  }

  private def addFunc2(name: Symbol, p1: Symbol = 'a, p2: Symbol = 'b)(f: (Double, Double) => Double) {
    _functions += name -> MathFunc2(name, p1, p2, f)
  }

}