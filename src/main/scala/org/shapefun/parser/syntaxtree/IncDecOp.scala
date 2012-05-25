package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.ParameterChecker

/**
 *
 */
case class IncDecOp(identifier: Symbol, increment: Boolean) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  def checkTypes() {
    // TODO: Get variable type from type checking context, check that it is numeric
  }

  def returnType(): Class[_] = {
    // TODO: Get variable type from type checking context
    null
  }

  def calculate(context: Context): AnyRef = {
    val oldBoxedVal: AnyRef = context.getVariable(identifier)
    val oldValue = Double.unbox(oldBoxedVal)
    val newValue = Double.box(if (increment) oldValue + 1 else oldValue - 1)
    context.setVariable(identifier, newValue)
    oldBoxedVal
  }
}