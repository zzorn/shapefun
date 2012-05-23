package org.shapefun.parser

import func.Func

/**
 *
 */
case class SimpleClassInfo(classType: Class[_], functions: List[Func] = Nil) extends ClassInfo {

  private val functionsByName  = Map[Symbol, Func]() ++ (functions map (f => f.identifier -> f))

  def getFunction(identifier: Symbol): Func = {
    functionsByName(identifier)
  }
}