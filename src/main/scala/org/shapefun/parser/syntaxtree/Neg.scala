package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class Neg(expr: Expr) extends Expr {

  override def checkTypes() {
    ensureIsAssignable(Num.Class, expr)
  }

  def returnType() = Num.Class

  def calculate(context: Context): AnyRef = {
    val rightVal = expr.calculate(context)

    Double.box( -Double.unbox(rightVal))
  }

}