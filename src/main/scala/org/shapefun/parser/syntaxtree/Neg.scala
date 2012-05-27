package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{NumKind, Kind, Context}


/**
 *
 */
case class Neg(expr: Expr) extends Expr {


  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(NumKind, expr, staticContext)
    NumKind
  }

  def calculate(context: Context): AnyRef = {
    val rightVal = expr.calculate(context)

    Double.box( -Double.unbox(rightVal))
  }

}