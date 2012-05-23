package org.shapefun.parser.func

import org.shapefun.parser.CalculationError
import org.shapefun.parser.syntaxtree.Expr

/**
 *
 */
trait Func {

  def identifier: Symbol

  def parameters: List[Parameter]

  def returnType: Class[_]

  def checkTypes(params: List[Expr]) {
    // Check parameter size
    if (parameters.size != params.size)
      throw new CalculationError("Invalid number of parameters for function " + identifier.name + ", " +
                                 "expected "+parameters.size + " but got "+params.size)

    // Check parameter types
    parameters zip params foreach {p =>
      val parameterInfo: Parameter = p._1
      val parameterExpr: Expr     = p._2
      if (!parameterInfo.kind.isAssignableFrom(parameterExpr.returnType()))
        throw new CalculationError("Can not assign a value of type '"+parameterExpr.returnType()+"' " +
          "to the parameter '"+parameterInfo.name.name+"', expected value of type '"+parameterInfo.kind+"'")
    }
  }

  def invoke(parameters: List[AnyRef]): AnyRef

}

case class Parameter(name: Symbol, kind: Class[_] /* , defaultValue: Expr */)