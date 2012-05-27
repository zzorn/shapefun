package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{UnitKind, Kind, CalculationError, Context}


/**
 *
 */
trait Expr {

  private var _kind: Kind = null

  final def kind: Kind = _kind

  final def calculateTypes(staticContext: StaticContext): Kind = {
    val k = kind
    if (k != null) k
    else {
      _kind = doCalculateTypes(staticContext: StaticContext)
      _kind
    }
  }

  protected def doCalculateTypes(staticContext: StaticContext): Kind //= UnitKind

  def calculate(context: Context): AnyRef

  def ensureExprIsAssignableTo(expectedKind: Kind, expr: Expr, staticContext: StaticContext): Kind = {
    val kind = expr.calculateTypes(staticContext)
    ensureKindIsAssignableToKind(expectedKind, kind, expr, staticContext)
  }

  def ensureKindIsAssignableToKind(expectedKind: Kind, kind: Kind, expr: Expr, staticContext: StaticContext): Kind = {
    if (!expectedKind.isAssignableFrom(kind)) reportTypeError("Expression should be of type '" + expectedKind + "', but it was of type '" + kind + "'", expr, staticContext)
    kind
  }

  def reportTypeError(msg: String, problematicExpr: Expr, staticContext: StaticContext) {
    throw new CalculationError("Error while checking types: " + msg + ", at: " + problematicExpr)
  }

  /*
  protected def ensureIsAssignable(required: Kind, expression: Expr) {
    if (!required.isAssignableFrom(expression.returnType()))
      throw new CalculationError("Expected type assignable to '"+required+"', but got " + expression.returnType())
  }
  */



}