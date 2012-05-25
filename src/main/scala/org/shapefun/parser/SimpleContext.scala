package org.shapefun.parser

import func.Func
import org.shapefun.utils.ParameterChecker


/**
 *
 */
case class SimpleContext(externalValues: Map[Symbol, AnyRef] = Map(),
                         functions: List[Func] = Nil,
                         classTypes: List[ClassInfo] = Nil ) extends Context {

  private var addedVariables = Map[Symbol, AnyRef]()

  private val functionsByName  = Map[Symbol, Func]() ++ (functions map (f => f.identifier -> f))
  private val classTypesByType = Map[Class[_], ClassInfo]() ++ (classTypes map (t => t.classType -> t))

  def hasVariable(identifier: Symbol) = externalValues.contains(identifier) || addedVariables.contains(identifier)
  def hasVariableInCurrentScope(identifier: Symbol) = addedVariables.contains(identifier)

  def getVariable(identifier: Symbol): AnyRef = {
    if (!hasVariable(identifier)) throw new CalculationError("No variable with the name '" + identifier.name + "' found")

    externalValues.getOrElse(identifier, addedVariables(identifier))
  }

  def addVariable(identifier: Symbol, value: AnyRef) {
    ParameterChecker.requireNotNull(value, 'value)
    if (hasVariableInCurrentScope(identifier)) throw new CalculationError("A variable with the name '" + identifier.name + "' already exists in the current scope")

    addedVariables += identifier -> value
  }

  def setVariable(identifier: Symbol, value: AnyRef) {
    ParameterChecker.requireNotNull(value, 'value)
    if (!addedVariables.contains(identifier)) throw new CalculationError("No editable variable with the name '" + identifier.name + "' found")

    addedVariables += identifier -> value
  }


  def getFunction(identifier: Symbol): Func = {
    if (!functionsByName.contains(identifier)) throw new CalculationError("No function with the name '" + identifier.name + "' found")

    functionsByName(identifier)
  }

  def getFunctionOnObject(hostType: Class[_], identifier: Symbol): Func = {
    classTypesByType(hostType).getFunction(identifier)
  }
}