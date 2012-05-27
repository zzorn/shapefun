package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import org.shapefun.parser.{AnyRefKind, Kind, Context}

/**
 *
 */
case class VarAssignment(identifier: Symbol, assignmentOp: Symbol, valueExpr: Expr) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  val allowedOps = Set('=, '+=, '-=, '*=, '/=)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    // TODO: Check context, ensure var exists and is correct type
    val varType = AnyRefKind

    ensureExprIsAssignableTo(varType, valueExpr, staticContext)

    if (!allowedOps.contains(assignmentOp)) reportTypeError("Assignment operator '"+assignmentOp.name+"' is not allowed", this, staticContext)

    varType
  }

  def calculate(context: Context): AnyRef = {

    val value = valueExpr.calculate(context)

    val newValue = if (assignmentOp == '=) value
    else {
      val oldValue = Double.unbox(context.getVar(identifier))
      val numValue = Double.unbox(value)
      Double.box(assignmentOp match {
        case '+= => oldValue + numValue
        case '-= => oldValue - numValue
        case '*= => oldValue * numValue
        case '/= => oldValue / numValue
      })
    }

    context.setVar(identifier, newValue)

    newValue
  }
}