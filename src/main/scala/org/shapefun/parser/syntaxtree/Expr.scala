package org.shapefun.parser.syntaxtree

/**
 *
 */
trait Expr {

  def calculate(context: Context): Any

}