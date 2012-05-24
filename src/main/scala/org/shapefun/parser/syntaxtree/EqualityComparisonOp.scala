package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class EqualityComparisonOp(a: Expr, op: Symbol, b: Expr) extends Expr {

  override def checkTypes() {
    ensureIsAssignable(Num.Class, a)
    ensureIsAssignable(Num.Class, b)
  }

  def returnType() = Bool.Class

  def calculate(context: Context): AnyRef = {
    val aVal = a.calculate(context)
    val bVal = b.calculate(context)

    Boolean.box(compare(aVal, op, bVal))
  }

  private def compare(v1: AnyRef, op: Symbol, v2: AnyRef): Boolean = {
    op match {
      case '==  =>  v1 == v2
      case '!=  =>  v1 != v2
      case _ => throw new Error("Unknown comparison operator '"+op.name+"'.")
    }
  }

}
