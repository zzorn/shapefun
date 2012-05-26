package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{CalculationError, Context}
import org.shapefun.parser.defs.FunDef


/**
 *
 */
case class CallExpr(hostObj: Option[Expr], identifier: Symbol, parameters: List[Expr]) extends Expr {

  override def checkTypes() {
    if (hostObj.isDefined) hostObj.get.checkTypes()

    parameters foreach (_.checkTypes())

    // TODO: Get function from context, call checking
  }

  /*
  def checkTypes(params: List[Expr]) {
    // Check parameter size
    if (parameters.size != params.size)
      throw new CalculationError("Invalid number of parameters for function " + identifier.name + ", " +
                                 "expected "+parameters.size + " but got "+params.size)

    // Check parameter types
    parameters zip params foreach {p =>
      val parameterInfo: ParameterInfo = p._1
      val parameterExpr: Expr     = p._2
      if (!parameterInfo.kind.isAssignableFrom(parameterExpr.returnType()))
        throw new CalculationError("Can not assign a value of type '"+parameterExpr.returnType()+"' " +
          "to the parameter '"+parameterInfo.name.name+"', expected value of type '"+parameterInfo.kind+"'")
    }
  }
*/

  def returnType(): Class[_] = {
    // TODO: Get function from context, get return type
    null
  }


  def calculate(context: Context): AnyRef = {
    hostObj match {
      case None =>
        val fun = context.getFun(identifier)
        val args = buildParamValuesByName(fun, parameters, context)
        fun.invoke(args)

      case Some(hostExpr) =>
        val hostValue = hostExpr.calculate(context)
        val extFun = context.getExtFun((hostValue.getClass, identifier))
        val args = buildParamValuesByName(extFun, parameters, context, hostValue)
        extFun.invoke(args)
    }
  }

  // TODO: Call this in syntax checking phase
  private def buildParametersByName(funDef: FunDef, parameters: List[Expr], hostExpr: Expr = null): Map[Symbol, Expr] = {
    val paramExprs = if (hostExpr != null) hostExpr::parameters else parameters
    (funDef.parameters zip paramExprs map(infoAndExpr => infoAndExpr._1.name -> infoAndExpr._2 )).toMap
  }

  private def buildParamValuesByName(funDef: FunDef, parameters: List[Expr], callContext: Context, hostValue: AnyRef = null): Map[Symbol, AnyRef] = {
    val paramValues = parameters map {_.calculate(callContext)}
    val allParamValues = if (hostValue != null) hostValue::paramValues else paramValues
    (funDef.parameters zip allParamValues map(infoAndExpr => infoAndExpr._1.name -> infoAndExpr._2 )).toMap
  }
}