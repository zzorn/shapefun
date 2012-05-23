package org.shapefun.parser



/**
 *
 */
case class SimpleContext(values: Map[Symbol, Any] = Map()) extends Context {

  def hasVariable(identifier: Symbol) = values.contains(identifier)

  def getVariable(identifier: Symbol): Any = {
    if (!values.contains(identifier)) throw new CalculationError("No variable with the name '" + identifier.name + "' found")

    values(identifier)
  }

}