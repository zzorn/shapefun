package org.shapefun.parser

import func.Func
import syntaxtree.Expr

/**
 *
 */
trait Context {

  def hasVariable(identifier: Symbol): Boolean

  def getVariable(identifier: Symbol): AnyRef

  def addVariable(identifier: Symbol, value: AnyRef)

  def setVariable(identifier: Symbol, value: AnyRef)

  def getFunction(identifier: Symbol): Func

  def getFunctionOnObject(hostType: Class[_], symbol: Symbol): Func

}