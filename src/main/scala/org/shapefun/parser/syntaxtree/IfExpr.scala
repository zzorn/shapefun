package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class IfExpr(condition: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr {

  def checkTypes() {
    thenExpr.checkTypes()
    elseExpr.checkTypes()
  }

  def returnType() = getCommonSuperType(thenExpr.returnType(), elseExpr.returnType())

  def calculate(context: Context): AnyRef = {
    if (Boolean.unbox(condition.calculate(context)))
      thenExpr.calculate(context)
    else
      elseExpr.calculate(context)
  }
}