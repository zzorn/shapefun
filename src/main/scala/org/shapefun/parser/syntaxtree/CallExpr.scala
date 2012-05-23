package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class CallExpr(hostObj: Option[Expr], identifier: Symbol, parameters: List[Expr]) extends Expr {

  override def checkTypes() {
    // TODO: Get function from context, call checking
  }

  def returnType(): Class[_] = {
    // TODO: Get function from context, get return type
    null
  }


  def calculate(context: Context): AnyRef = {
    val paramValues: List[AnyRef] = parameters map {p => p.calculate(context)}

    hostObj match {
      case Some(hostExpr) =>
        val hostValue = hostExpr.calculate(context)
        context.getFunctionOnObject(hostValue.getClass, identifier).invokeOnObject(hostValue, paramValues)

      case None =>
        context.getFunction(identifier).invoke(paramValues)
    }

  }
}