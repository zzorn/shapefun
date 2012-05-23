package org.shapefun.parser.syntaxtree

/**
 *
 */
case class BinaryOp(operation: Symbol, left: Expr, right: Expr) extends Expr {

  def calculate(context: Context): Any = {
    val leftVal = left.calculate(context)
    val rightVal = right.calculate(context)

    operation match {
      case 'plus  => leftVal.asInstanceOf[Double] + rightVal.asInstanceOf[Double]
      case 'minus => leftVal.asInstanceOf[Double] - rightVal.asInstanceOf[Double]
      case 'mul   => leftVal.asInstanceOf[Double] * rightVal.asInstanceOf[Double]
      case 'div   => leftVal.asInstanceOf[Double] / rightVal.asInstanceOf[Double]
      case _ => throw new Error("Unknown operator '"+operation.name+"'.")
    }
  }

}