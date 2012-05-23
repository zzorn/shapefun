package org.shapefun.parser

/**
 *
 */
trait Context {

  def hasVariable(identifier: Symbol): Boolean

  def getVariable(identifier: Symbol): Any

}