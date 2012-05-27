package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{BoolKind, NumKind, Kind, Context}


/**
 *
 */
case class Not(expr: Expr) extends Expr {

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(BoolKind, expr, staticContext)
    BoolKind
  }

  def calculate(context: Context): AnyRef = {
    val value = expr.calculate(context)

    Boolean.box( !Boolean.unbox(value))
  }

}