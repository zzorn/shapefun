package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{CalculationError, Context}


/**
 *
 */
trait Expr {

  def returnType(): Class[_]

  def checkTypes()

  def calculate(context: Context): AnyRef


  protected def ensureIsAssignable(required: Class[_], expression: Expr) {
    if (!required.isAssignableFrom(expression.returnType()))
      throw new CalculationError("Expected type assignable to '"+required+"', but got " + expression.returnType())
  }

}