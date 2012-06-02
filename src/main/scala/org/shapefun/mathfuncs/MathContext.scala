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

  _context.addNumFun1('abs) { abs(_) }
  _context.addNumFun1('signum) { signum(_) }
  _context.addNumFun1('ceil) { ceil(_) }
  _context.addNumFun1('floor) { floor(_) }
  _context.addNumFun1('round) { round(_) }

  _context.addNumFun1('toDegrees) { toDegrees(_) }
  _context.addNumFun1('toRadians) { toRadians(_) }

  _context.addNumFun2('max) { max(_, _) }
  _context.addNumFun2('min) { min(_, _) }

  _context.addNumFun2('pow) { pow(_, _) }
  _context.addNumFun1('sqrt) { sqrt(_) }

  _context.addNumFun1('log) { log(_) }
  _context.addNumFun1('log10) { log10(_) }
  _context.addNumFun1('exp) { exp(_) }

  _context.addNumFun1('sin)  { sin(_)  }
  _context.addNumFun1('cos)  { cos(_)  }
  _context.addNumFun1('tan)  { tan(_)  }
  _context.addNumFun1('asin) { asin(_) }
  _context.addNumFun1('acos) { acos(_) }
  _context.addNumFun1('atan) { atan(_) }

  _context.addNumFun0('random) { _context.getValue('randomNumberGenerator).asInstanceOf[Random].nextDouble() }







}