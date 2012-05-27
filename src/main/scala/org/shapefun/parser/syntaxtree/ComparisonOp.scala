package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{BoolKind, NumKind, Kind, Context}


/**
 *
 */
case class ComparisonOp(a: Expr, op1: Symbol, b: Expr, op2: Symbol = null, c: Expr = null) extends Expr {

  val allowedOps = Set('>, '>=, '<, '<=)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(NumKind, a, staticContext)
    ensureExprIsAssignableTo(NumKind, b, staticContext)
    if (c != null) ensureExprIsAssignableTo(NumKind, c, staticContext)

    if (!allowedOps.contains(op1)) reportTypeError("Operator '"+op1.name+"' is not allowed", this, staticContext)
    if (op2 != null && !allowedOps.contains(op2)) reportTypeError("Operator '"+op1.name+"' is not allowed", this, staticContext)
    if (op2 != null && c == null) reportTypeError("Second operator was present, but third operand was missing", this, staticContext)

    BoolKind
  }

  def returnType() = Bool.Class

  def calculate(context: Context): AnyRef = {
    val aVal = Double.unbox(a.calculate(context))
    val bVal = Double.unbox(b.calculate(context))

    if (op2 != null) {
      val cVal = Double.unbox(c.calculate(context))
      Boolean.box(compare(aVal, op1, bVal) && compare(bVal, op2, cVal))
    }
    else {
      Boolean.box(compare(aVal, op1, bVal))
    }
  }

  private def compare(v1: Double, op: Symbol, v2: Double): Boolean = {
    // Use epsilon to compensate for double calculation inaccuracies.
    op match {
      case '>   =>  v1 >  v2 + Num.Epsilon
      case '>=  =>  v1 >= v2 - Num.Epsilon
      case '<   =>  v1 <  v2 - Num.Epsilon
      case '<=  =>  v1 <= v2 + Num.Epsilon
    }
  }

}
