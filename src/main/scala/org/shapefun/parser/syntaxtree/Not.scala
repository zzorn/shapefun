package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class Not(expr: Expr) extends Expr {

  override def checkTypes() {
    expr.checkTypes()
    ensureIsAssignable(Bool.Class, expr)
  }

  def returnType() = Bool.Class

  def calculate(context: Context): AnyRef = {
    val value = expr.calculate(context)

    Boolean.box( !Boolean.unbox(value))
  }

}