package org.shapefun.parser.func

/**
 *
 */
case class SimpleFunc(identifier: Symbol,
                      parameters: List[ParameterInfo],
                      returnType: Class[_],
                      func: List[AnyRef] => AnyRef) extends Func {

  def invoke(parameters: List[AnyRef]): AnyRef = {
    func(parameters)
  }
}