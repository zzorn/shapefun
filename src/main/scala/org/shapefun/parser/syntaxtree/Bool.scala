package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import org.shapefun.parser.Context

/**
 *
 */
case class Bool(value: java.lang.Boolean) extends Expr {
  ParameterChecker.requireNotNull(value, 'value)

  def calculate(context: Context) = value
  def returnType() = Bool.Class
  override def checkTypes() {}
}

object True extends Bool(java.lang.Boolean.TRUE)

object False extends Bool(java.lang.Boolean.FALSE)

object Bool {
  type BoolType = java.lang.Boolean
  val Class = classOf[java.lang.Boolean]
}