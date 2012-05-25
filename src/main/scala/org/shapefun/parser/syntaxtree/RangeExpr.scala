package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.StepRange

/**
 *
 */
case class RangeExpr(start:Expr, end: Expr, inclusive: Boolean = false, step: Expr = null, steps: Expr = null) extends Expr {

  def returnType() = classOf[StepRange]

  def checkTypes() {
    start.checkTypes()
    end.checkTypes()
    if (step != null) step.checkTypes()
    if (steps != null) steps.checkTypes()

    ensureIsAssignable(Num.Class, start)
    ensureIsAssignable(Num.Class, end)
    if (step != null) ensureIsAssignable(Num.Class, step)
    if (steps != null) ensureIsAssignable(Num.Class, steps)
  }

  def calculate(context: Context): AnyRef = {
    val startVal = Double.unbox(start.calculate(context))
    val endVal = Double.unbox(end.calculate(context))

    if (step == null && steps == null) {
      StepRange(startVal, endVal, 1, inclusive)
    }
    else if (step != null) {
      val stepVal = Double.unbox(step.calculate(context))
      StepRange(startVal, endVal, stepVal, inclusive)
    }
    else {
      val stepsVal = Double.unbox(steps.calculate(context))
      StepRange.withSteps(startVal, endVal, stepsVal, inclusive)
    }
  }
}
