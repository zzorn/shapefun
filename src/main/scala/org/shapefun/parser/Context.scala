package org.shapefun.parser

import defs._
import syntaxtree.StaticContext


/**
 *
 */
trait Context {

  /**
   * @return True if definition exists locally or inherited from parent context
   */
  def hasDef(name: Symbol): Boolean

  def hasVal(name: Symbol): Boolean = hasDef(name) && classOf[ValDef].isInstance(getDef(name))
  def hasVar(name: Symbol): Boolean = hasDef(name) && classOf[VarDef].isInstance(getDef(name))
  def hasFun(name: Symbol): Boolean = hasDef(name) && classOf[FunDef].isInstance(getDef(name))
  def hasValue(name: Symbol): Boolean = hasDef(name) && classOf[ValueDef].isInstance(getDef(name))
  def hasExtFun(key: (Class[_], Symbol)): Boolean

  /**
   * Adds a definition, throws an exception if it is already defined locally
   */
  def addDef(definition: Def)

  def addVal(name: Symbol, value: AnyRef) {addDef(new ValDef(name, value))}
  def addVal(name: Symbol, value: Double) {addDef(new ValDef(name, Double.box(value)))}
  def addVar(name: Symbol, value: AnyRef) {addDef(new VarDef(name, value))}
  def addVar(name: Symbol, value: Double) {addDef(new VarDef(name, Double.box(value)))}
  def addFun(funDef: FunDef) {addDef(funDef)}
  def addExtFun(hostType: Class[_], funDef: FunDef)

  /**
   * @return the definition with the specified name, or throw exception if not found
   */
  def getDef(name: Symbol): Def

  def getVal(name: Symbol): AnyRef = getDef(name).asInstanceOf[ValDef].value
  def getVar(name: Symbol): AnyRef = getDef(name).asInstanceOf[VarDef].value
  def getValue(name: Symbol): AnyRef = getDef(name).asInstanceOf[ValueDef].value
  def getFun(name: Symbol): FunDef = getDef(name).asInstanceOf[FunDef]
  def getExtFun(key: (Class[_], Symbol)): FunDef

  /**
   * Sets variable, throws exception if not found.
   * Throws class cast if it is not a variable.
   */
  def setVar(name: Symbol, value: AnyRef)

  /**
   * @return parent context, or null if this context has no parent
   */
  def parent: Context

  /**
   * @return a new context with this context as parent
   */
  def createSubContext(): Context

  def createStaticContext(): StaticContext

}