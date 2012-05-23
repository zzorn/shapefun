package org.shapefun.parser.func

import org.shapefun.parser.syntaxtree.Num

/**
 *
 */
case class MathFunc2(identifier: Symbol, param1Name: Symbol, param2Name: Symbol, func: (Double, Double)=> Double) extends Func {

  def parameters = List(
    ParameterInfo(param1Name, Num.Class),
    ParameterInfo(param2Name, Num.Class)
  )

  def returnType = Num.Class

  def invoke(parameters: List[AnyRef]): AnyRef = {
    Double.box(func(
      Double.unbox(parameters(0)),
      Double.unbox(parameters(1))
    ))
  }

}
