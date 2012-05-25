package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class Block(statements: List[SyntaxNode]) extends Expr {

  def returnType(): Class[_] = if (statements.isEmpty) classOf[Unit] else statements.last.returnType()

  def checkTypes() {
    statements foreach {_.checkTypes()}
  }

  def calculate(context: Context): AnyRef = {
    var result: AnyRef = Unit
    statements foreach {s => result = s.calculate(context)}
    result
  }

}