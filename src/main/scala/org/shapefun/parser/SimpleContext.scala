package org.shapefun.parser

import func.Func


/**
 *
 */
case class SimpleContext(values: Map[Symbol, AnyRef] = Map(), functions: Map[Symbol, Func] = Map()) extends Context {

  def hasVariable(identifier: Symbol) = values.contains(identifier)

  def getVariable(identifier: Symbol): AnyRef = {
    if (!values.contains(identifier)) throw new CalculationError("No variable with the name '" + identifier.name + "' found")

    values(identifier)
  }


  def getFunction(identifier: Symbol): Func = {
    if (!functions.contains(identifier)) throw new CalculationError("No function with the name '" + identifier.name + "' found")

    functions(identifier)
  }

}