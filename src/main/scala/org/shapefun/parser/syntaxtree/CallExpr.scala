package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class CallExpr(identifier: Symbol, parameters: List[Expr]) extends Expr {

  override def checkTypes() {
    // TODO: Get function from context, call checking
  }

  def returnType(): Class[_] = {
    // TODO: Get function from context, get return type
    null
  }


  def calculate(context: Context): AnyRef = {
    val paramValues: List[AnyRef] = parameters map {p => p.calculate(context)}
    context.getFunction(identifier).invoke(paramValues)
  }
}