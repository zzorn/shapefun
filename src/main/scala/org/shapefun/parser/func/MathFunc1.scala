package org.shapefun.parser.func

import org.shapefun.parser.syntaxtree.Num

/**
 *
 */
case class MathFunc1(identifier: Symbol, param1Name: Symbol, func: (Double) => Double) extends Func {

  def parameters = List(
    ParameterInfo(param1Name, Num.Class)
  )

  def returnType = Num.Class

  def invoke(parameters: List[AnyRef]): AnyRef = {
    Double.box(func(
      Double.unbox(parameters(0))
    ))
  }

}
