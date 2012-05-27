package org.shapefun.parser.syntaxtree

import org.shapefun.parser.defs.{ExprFunDef, ParamInfo}
import org.shapefun.parser._

/**
 *
 */
case class FunDefExpr(name: Symbol, params: List[ParamInfo], specifiedReturnKind: Kind, body: Expr) extends Expr {

  private var _paramTypes: Map[Symbol, Kind] = null
  private var _returnType: Kind = null

  def paramTypes: Map[Symbol, Kind] = Map()
  def returnType: Kind = _returnType

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    // Calculate parameter types
    _paramTypes = Map()
    params foreach {p =>
      var k= if (p.defaultValue != null) p.defaultValue.calculateTypes(staticContext) else null
      if (p.kind != null) k = p.kind
      if (k == null) k = NumKind // Default to number types if type info missing
      _paramTypes += p.name -> k
    }

    // Calculate return type
    val bodyKind = body.calculateTypes(staticContext)
    if (specifiedReturnKind != null) ensureKindIsAssignableToKind(specifiedReturnKind, bodyKind, body, staticContext)

    _returnType = if (specifiedReturnKind != null) specifiedReturnKind else bodyKind

    UnitKind
  }

  def calculate(context: Context): AnyRef = {
    context.addDef(ExprFunDef(name, params, kind, body, context))
    Unit
  }
}