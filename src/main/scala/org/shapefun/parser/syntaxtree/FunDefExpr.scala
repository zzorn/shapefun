package org.shapefun.parser.syntaxtree

import org.shapefun.parser.defs.{ExprFunDef, ParamInfo}
import org.shapefun.parser.{Kind, Context}

/**
 *
 */
case class FunDefExpr(name: Symbol, params: List[ParamInfo], kind: Kind, body: Expr) extends Expr {

  def checkTypes() {
    body.checkTypes()
  }

  def returnType() = classOf[Unit]

  def calculate(context: Context): AnyRef = {
    context.addDef(ExprFunDef(name, params, kind, body, context))
    Unit
  }
}