package org.shapefun.parser.syntaxtree

/**
 *
 */
case class Num(value: Double) extends Expr {
  def calculate(context: Context): Any = value
}