package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class Num(value: Double) extends Expr {
  def calculate(context: Context): Any = value
}