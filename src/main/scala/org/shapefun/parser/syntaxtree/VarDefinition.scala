package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.ParameterChecker

/**
 *
 */
case class VarDefinition(identifier: Symbol, initialValue: Expr) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  def checkTypes() {
    initialValue.checkTypes()
  }

  def returnType(): Class[_] = initialValue.returnType()

  def calculate(context: Context): AnyRef = {
    val value = initialValue.calculate(context)
    context.addVar(identifier, value)
    Unit
  }
}