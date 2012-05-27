package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import org.shapefun.parser.{BoolKind, Context}

/**
 *
 */
case class Bool(value: java.lang.Boolean) extends Expr {
  ParameterChecker.requireNotNull(value, 'value)

  protected def doCalculateTypes(staticContext: StaticContext) = BoolKind

  def calculate(context: Context) = value
}

object True extends Bool(java.lang.Boolean.TRUE)

object False extends Bool(java.lang.Boolean.FALSE)

object Bool {
  val Kind = BoolKind
  type BoolType = java.lang.Boolean
  val Class = classOf[java.lang.Boolean]
}