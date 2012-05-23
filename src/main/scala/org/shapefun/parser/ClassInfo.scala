package org.shapefun.parser

import func.Func

/**
 *
 */
trait ClassInfo {

  def classType: Class[_]

  def getFunction(identifier: Symbol): Func

}