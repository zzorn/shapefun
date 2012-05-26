package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.ParameterChecker

/**
 *
 */
case class ValueRefExpr(identifier: Symbol) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  override def checkTypes() {}

  def returnType(): Class[_] = {
    // TODO: Can this be calculated? -> need to calculate the context
    null
  }

  def calculate(context: Context): AnyRef = {
    context.getValue(identifier)
  }

}