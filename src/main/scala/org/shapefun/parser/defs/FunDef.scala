package org.shapefun.parser.defs

import org.shapefun.parser.syntaxtree.Expr
import org.shapefun.parser.{CalculationError, Context}
import jme3tools.navigation.StringUtil
import org.shapefun.utils.{ParameterChecker, StringUtils}

/**
 *
 */
trait FunDef extends Def {

  def invoke(params: Map[Symbol, AnyRef]): AnyRef

  def parameters: List[ParamInfo]

  protected def checkParameters(parameters: List[ParamInfo]) {
    ParameterChecker.requireNotNull(parameters, 'parameters)

    val reservedParams: Set[Symbol] = Set('this)
    var checkedParams: Set[Symbol] = Set()

    parameters foreach {param =>
      val paramName = param.name
      if (!StringUtils.isIdentifier(paramName)) throw new IllegalArgumentException("The parameter name '"+paramName+"' is not an identifier")
      if (checkedParams.contains(paramName)) throw new IllegalArgumentException("The parameter name '"+paramName+"' was used for two parameters")
      if (reservedParams.contains(paramName)) throw new IllegalArgumentException("The parameter name '"+paramName+"' is reserved")
      checkedParams += paramName
    }
  }
}

case class ExternalFunDef(name: Symbol, parameters: List[ParamInfo], returnType: Class[_], func: Map[Symbol, AnyRef] => AnyRef) extends FunDef {
  ParameterChecker.requireIsIdentifier(name, 'name)
  ParameterChecker.requireNotNull(returnType, 'returnType)
  ParameterChecker.requireNotNull(func, 'func)
  checkParameters(parameters)

  def invoke(params: Map[Symbol, AnyRef]) = func(params)
}


/**
 *
 */
case class ExprFunDef(name: Symbol, parameters: List[ParamInfo], body: Expr, definitionContext: Context) extends FunDef {
  ParameterChecker.requireIsIdentifier(name, 'name)
  ParameterChecker.requireNotNull(returnType, 'returnType)
  ParameterChecker.requireNotNull(body, 'body)
  ParameterChecker.requireNotNull(definitionContext, 'definitionContext)
  checkParameters(parameters)


  def returnType: Class[_] = body.returnType()

  def invoke(arguments: Map[Symbol, AnyRef]): AnyRef = {
    // Create execution context that includes the parameters
    val functionContext: Context = definitionContext.createSubContext()

    // Get values for parameters
    parameters foreach {param =>
      val parameterName = param.name
      val parameterValue: AnyRef = if (arguments.contains(parameterName)) {
        // Get argument value
        arguments(parameterName)
      }
      else if (param.defaultValue != null) {
        // Use default value
        param.defaultValue.calculate(definitionContext)
      }
      else {
        throw new CalculationError("No value passed for parameter " +parameterName.name+ " when calling function "+name.name)
      }

      functionContext.addVal(parameterName, parameterValue)
    }

    // Calculate function body with the given arguments
    body.calculate(functionContext)
  }
}


case class ParamInfo(name: Symbol, kind: Class[_], defaultValue: Expr = null)