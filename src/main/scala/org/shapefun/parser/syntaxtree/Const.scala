package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class Const(value: AnyRef) extends Expr{

  def returnType() = value.getClass

  def checkTypes() {}

  def calculate(context: Context) = value
}