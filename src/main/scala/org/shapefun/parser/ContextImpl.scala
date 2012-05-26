package org.shapefun.parser

import defs.{VarDef, Def, FunDef}
import org.shapefun.utils.ParameterChecker


/**
 *
 */
case class ContextImpl(parent: Context = null) extends Context {

  private var localDefs = Map[Symbol, Def]()
  private var localExtFuns = Map[(Class[_], Symbol), FunDef]()

  def hasDef(name: Symbol): Boolean = {
    localDefs.contains(name) || (if (parent != null) parent.hasDef(name) else false)
  }

  def addDef(definition: Def) {
    ParameterChecker.requireNotNull(definition, 'definition)
    val defName: Symbol = definition.name
    ParameterChecker.requireIsIdentifier(defName, 'definitionName)
    if (localDefs.contains(defName)) throw new CalculationError("There is already a definition for '"+defName+"' in the local context")

    localDefs += defName -> definition
  }

  def getDef(name: Symbol): Def = {
    localDefs.getOrElse(name, if (parent != null) parent.getDef(name) else throw new CalculationError("No definition with the name '"+name.name+"' found."))
  }


  def setVar(name: Symbol, value: AnyRef) {
    if (localDefs.contains(name)) {
      localDefs(name).asInstanceOf[VarDef].value = value
    }
    else if (parent != null) {
      parent.setVar(name, value)
    }
    else {
      throw new CalculationError("No variable with the name '"+name.name+"' found.")
    }
  }

  def hasExtFun(key: (Class[_], Symbol)): Boolean = {
    localExtFuns.contains(key) || (if (parent != null) parent.hasExtFun(key) else false)
  }

  def getExtFun(key: (Class[_], Symbol)): FunDef = {
    localExtFuns.getOrElse(key, if (parent != null) parent.getExtFun(key) else throw new CalculationError("No extension function named '"+key._2+"' that applies to classes of type '"+key._1+"' found."))
  }


  def addExtFun(hostType: Class[_], funDef: FunDef) {
    ParameterChecker.requireNotNull(hostType, 'hostType)
    ParameterChecker.requireNotNull(funDef, 'funDef)
    val defName: Symbol = funDef.name
    ParameterChecker.requireIsIdentifier(defName, 'definitionName)
    if (!funDef.parameters.exists(_.name == 'self)) throw new CalculationError("Extension functions need to define a parameter named self.")

    localExtFuns += (hostType, defName) -> funDef
  }


  def createSubContext(): Context = {
    new ContextImpl(this)
  }


}