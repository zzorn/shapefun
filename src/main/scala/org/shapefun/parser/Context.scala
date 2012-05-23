package org.shapefun.parser

import func.Func
import syntaxtree.Expr

/**
 *
 */
trait Context {

  def hasVariable(identifier: Symbol): Boolean

  def getVariable(identifier: Symbol): AnyRef

  def getFunction(identifier: Symbol): Func

}