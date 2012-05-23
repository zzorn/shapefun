package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
case class UnaryOp(operation: Symbol, right: Expr) extends Expr {

  def calculate(context: Context): Any = {
    val rightVal = right.calculate(context)

    operation match {
      case 'minus => -rightVal.asInstanceOf[Double]
      case 'plus  =>  rightVal.asInstanceOf[Double]
      case _ => throw new Error("Unknown operation '"+operation.name+"'")
    }
  }

}