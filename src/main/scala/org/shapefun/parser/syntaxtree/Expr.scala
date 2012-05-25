package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{CalculationError, Context}


/**
 *
 */
trait Expr extends SyntaxNode {

  protected def ensureIsAssignable(required: Class[_], expression: Expr) {
    if (!required.isAssignableFrom(expression.returnType()))
      throw new CalculationError("Expected type assignable to '"+required+"', but got " + expression.returnType())
  }

  protected def getCommonSuperType(type1: Class[_], type2: Class[_]): Class[_] = {
    if (type1.isAssignableFrom(type2)) type1
    else if (type2.isAssignableFrom(type1)) type2
    else classOf[Object] // TODO: Find closest common super type
  }

}