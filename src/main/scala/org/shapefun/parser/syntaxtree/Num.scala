package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.ParameterChecker
import scala.Predef._

/**
 *
 */
case class Num(value: Double) extends Expr {
  ParameterChecker.requireNotNull(value, 'value)

  override def checkTypes() {}

  def returnType() = Num.Class

  def calculate(context: Context): AnyRef = Double.box(value)
}

object Num {
  val Epsilon: Double = 0.00000001

  type NumType = java.lang.Double
  val Class = classOf[java.lang.Double]
}