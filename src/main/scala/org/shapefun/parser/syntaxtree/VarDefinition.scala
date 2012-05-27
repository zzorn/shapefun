package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import org.shapefun.parser.{UnitKind, Kind, Context}

/**
 *
 */
case class VarDefinition(identifier: Symbol, initialValue: Expr, var varType: Kind = null) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    // TODO: Add var to context?

    if (varType != null) ensureExprIsAssignableTo(varType, initialValue, staticContext)
    else varType = initialValue.calculateTypes(staticContext)

    UnitKind
  }

  def calculate(context: Context): AnyRef = {
    val value = initialValue.calculate(context)
    context.addVar(identifier, value)
    Unit
  }
}