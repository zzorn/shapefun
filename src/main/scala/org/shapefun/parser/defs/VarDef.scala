package org.shapefun.parser.defs

/**
 *
 */
class VarDef(val name: Symbol, initialValue: AnyRef) extends ValueDef {

  var value: AnyRef = initialValue

  def returnType = initialValue.getClass
}