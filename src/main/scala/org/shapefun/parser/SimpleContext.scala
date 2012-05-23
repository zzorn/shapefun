package org.shapefun.parser

import func.Func


/**
 *
 */
case class SimpleContext(values: Map[Symbol, AnyRef] = Map(),
                         functions: List[Func] = Nil,
                         classTypes: List[ClassInfo] = Nil ) extends Context {

  private val functionsByName  = Map[Symbol, Func]() ++ (functions map (f => f.identifier -> f))
  private val classTypesByType = Map[Class[_], ClassInfo]() ++ (classTypes map (t => t.classType -> t))

  def hasVariable(identifier: Symbol) = values.contains(identifier)

  def getVariable(identifier: Symbol): AnyRef = {
    if (!values.contains(identifier)) throw new CalculationError("No variable with the name '" + identifier.name + "' found")

    values(identifier)
  }


  def getFunction(identifier: Symbol): Func = {
    if (!functionsByName.contains(identifier)) throw new CalculationError("No function with the name '" + identifier.name + "' found")

    functionsByName(identifier)
  }

  def getFunctionOnObject(hostType: Class[_], identifier: Symbol): Func = {
    classTypesByType(hostType).getFunction(identifier)
  }
}