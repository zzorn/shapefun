package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{UnitKind, Kind, Context}


/**
 *
 */
case class BlockExpr(statements: List[Expr]) extends Expr {

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    var resultKind: Kind = UnitKind
    statements foreach {s => resultKind = s.calculateTypes(staticContext)}
    resultKind
  }

  def calculate(context: Context): AnyRef = {
    var result: AnyRef = Unit
    statements foreach {s => result = s.calculate(context)}
    result
  }

}