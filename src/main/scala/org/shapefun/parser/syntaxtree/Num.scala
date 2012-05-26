package org.shapefun.parser.syntaxtree

import org.shapefun.utils.ParameterChecker
import scala.Predef._
import org.shapefun.parser.{Kind, Context}

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
  val Kind = new Kind('Num)
  type NumType = java.lang.Double
  val Class = classOf[java.lang.Double]
}