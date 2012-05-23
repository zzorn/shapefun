package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class VarRefExpr(identifier: Symbol) extends Expr {

  def calculate(context: Context): Any = {
    context.getVariable(identifier)
  }

}