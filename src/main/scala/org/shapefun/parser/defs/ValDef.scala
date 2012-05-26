package org.shapefun.parser.defs

/**
 *
 */
case class ValDef(name: Symbol, value: AnyRef) extends ValueDef {

  def returnType = null // TODO: Class[_] = value.getClass

}