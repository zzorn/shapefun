package org.shapefun.parser.syntaxtree

import org.shapefun.utils.StepRange
import org.shapefun.parser.{JavaKind, NumKind, Kind, Context}

/**
 *
 */
case class RangeExpr(start:Expr, end: Expr, inclusive: Boolean = false, step: Expr = null, steps: Expr = null) extends Expr {


  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(NumKind, start, staticContext)
    ensureExprIsAssignableTo(NumKind, end, staticContext)
    if (step != null) ensureExprIsAssignableTo(NumKind, step, staticContext)
    if (steps != null) ensureExprIsAssignableTo(NumKind, steps, staticContext)

    JavaKind(classOf[StepRange])
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
