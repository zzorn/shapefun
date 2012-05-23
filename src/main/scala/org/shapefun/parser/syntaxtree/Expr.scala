package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
trait Expr {

  def calculate(context: Context): Any

}