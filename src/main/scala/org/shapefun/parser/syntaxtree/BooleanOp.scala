package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class BooleanOp(a: Expr, op1: Symbol, b: Expr) extends Expr {

  override def checkTypes() {
    ensureIsAssignable(Num.Class, a)
    ensureIsAssignable(Num.Class, b)
  }

  def returnType() = Bool.Class

  def calculate(context: Context): AnyRef = {
    val aVal = Boolean.unbox(a.calculate(context))
    val bVal = Boolean.unbox(b.calculate(context))

    Boolean.box(compare(aVal, op1, bVal))
  }

  private def compare(v1: Boolean, op: Symbol, v2: Boolean): Boolean = {
    op match {
      case 'and =>  v1 && v2
      case 'or  =>  v1 || v2
      case 'xor =>  (!v1 && v2) || (v1 && !v2)
      case _ => throw new Error("Unknown comparison operator '"+op.name+"'.")
    }
  }

}