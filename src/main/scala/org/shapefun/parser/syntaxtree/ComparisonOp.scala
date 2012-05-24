package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class ComparisonOp(a: Expr, op1: Symbol, b: Expr, op2: Symbol = null, c: Expr = null) extends Expr {

  override def checkTypes() {
    ensureIsAssignable(Num.Class, a)
    ensureIsAssignable(Num.Class, b)
    if (c != null) ensureIsAssignable(Num.Class, c)
  }

  def returnType() = Bool.Class

  def calculate(context: Context): AnyRef = {
    val aVal = Double.unbox(a.calculate(context))
    val bVal = Double.unbox(b.calculate(context))
    val cVal = if (c != null) Double.unbox(c.calculate(context)) else 0

    if (op2 != null) {
      Boolean.box(compare(aVal, op1, bVal) && compare(bVal, op2, cVal))
    }
    else {
      Boolean.box(compare(aVal, op1, bVal))
    }
  }

  private def compare(v1: Double, op: Symbol, v2: Double): Boolean = {
    op match {
      case '>   =>  v1 >  v2
      case '>=  =>  v1 >= v2
      case '<   =>  v1 <  v2
      case '<=  =>  v1 <= v2
      case '==  =>  v1 == v2
      case '!=  =>  v1 != v2
      case _ => throw new Error("Unknown comparison operator '"+op.name+"'.")
    }
  }

}
