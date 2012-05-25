package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{CalculationError, Context}
import scala.Predef._


/**
 *
 */
case class NumberOp(operation: Symbol, left: Expr, right: Expr) extends Expr {

  override def checkTypes() {
    left.checkTypes()
    right.checkTypes()
    ensureIsAssignable(Num.Class, left)
    ensureIsAssignable(Num.Class, right)
  }

  def returnType() = Num.Class

  def calculate(context: Context): AnyRef = {
    val leftVal = left.calculate(context)
    val rightVal = right.calculate(context)

    operation match {
      case 'plus  => Double.box( Double.unbox(leftVal) + Double.unbox(rightVal))
      case 'minus => Double.box( Double.unbox(leftVal) - Double.unbox(rightVal))
      case 'mul   => Double.box( Double.unbox(leftVal) * Double.unbox(rightVal))
      case 'div   => Double.box( Double.unbox(leftVal) / Double.unbox(rightVal))
      case _ => throw new Error("Unknown operator '"+operation.name+"'.")
    }
  }

}