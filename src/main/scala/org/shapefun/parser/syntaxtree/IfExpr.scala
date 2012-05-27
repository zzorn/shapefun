package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{Kind, Context}


/**
 *
 */
case class IfExpr(condition: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr {

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    val thenKind = thenExpr.calculateTypes(staticContext)
    val elseKind = elseExpr.calculateTypes(staticContext)

    thenKind.commonSuperType(elseKind)
  }

  def calculate(context: Context): AnyRef = {
    if (Boolean.unbox(condition.calculate(context)))
      thenExpr.calculate(context)
    else
      elseExpr.calculate(context)
  }
}