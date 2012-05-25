package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.ParameterChecker

/**
 *
 */
case class VarAssignment(identifier: Symbol, assignmentOp: Symbol, valueExpr: Expr) extends SyntaxNode {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  def checkTypes() {
    valueExpr.checkTypes()
    // TODO: Get variable type from type checking context,check that value matches
  }

  def returnType(): Class[_] = {
    // TODO: Get variable type from type checking context
    null
  }

  def calculate(context: Context): AnyRef = {

    val value = valueExpr.calculate(context)

    val newValue = if (assignmentOp == '=) value
    else {
      val oldValue = Double.unbox(context.getVariable(identifier))
      val numValue = Double.unbox(value)
      Double.box(assignmentOp match {
        case '+= => oldValue + numValue
        case '-= => oldValue - numValue
        case '*= => oldValue * numValue
        case '/= => oldValue / numValue
      })
    }

    context.setVariable(identifier, newValue)

    newValue
  }
}