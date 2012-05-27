package org.shapefun.parser.syntaxtree

import scala.Predef._
import org.shapefun.parser._


/**
 *
 */
case class NumberOp(operation: Symbol, left: Expr, right: Expr) extends Expr {

  val allowedOps = Set('plus, 'minus, 'mul, 'div)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(NumKind, left, staticContext)
    ensureExprIsAssignableTo(NumKind, right, staticContext)

    if (!allowedOps.contains(operation)) reportTypeError("Operator '"+operation.name+"' is not allowed", this, staticContext)

    NumKind
  }


  def calculate(context: Context): AnyRef = {
    val leftVal = left.calculate(context)
    val rightVal = right.calculate(context)

    operation match {
      case 'plus  => Double.box( Double.unbox(leftVal) + Double.unbox(rightVal))
      case 'minus => Double.box( Double.unbox(leftVal) - Double.unbox(rightVal))
      case 'mul   => Double.box( Double.unbox(leftVal) * Double.unbox(rightVal))
      case 'div   => Double.box( Double.unbox(leftVal) / Double.unbox(rightVal))
    }
  }

}