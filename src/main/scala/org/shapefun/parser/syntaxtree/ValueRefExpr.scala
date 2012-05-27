package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import org.shapefun.parser.{AnyRefKind, Kind, Context}

/**
 *
 */
case class ValueRefExpr(identifier: Symbol) extends Expr {
  ParameterChecker.requireIsIdentifier(identifier, 'identifier)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    // TODO: Get type of val from static context?

    null
  }

  def calculate(context: Context): AnyRef = {
    context.getValue(identifier)
  }

}