package org.shapefun.parser.syntaxtree

import org.shapefun.parser.defs.FunDef
import org.shapefun.parser.{UnitKind, Kind, CalculationError, Context}


/**
 *
 */
case class CallExpr(hostObj: Option[Expr], identifier: Symbol, arguments: List[CallArg]) extends Expr {


  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    if (hostObj.isDefined) hostObj.get.calculateTypes(staticContext)

    arguments foreach (_.expr.calculateTypes(staticContext))

    // TODO: Get function from context, call checking
    // TODO: Check that named arguments have correct names

    // TODO: Get function return type
    val functionReturnType: Kind = null

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

    functionReturnType
  }

  def calculate(context: Context): AnyRef = {
    hostObj match {
      case None =>
        val fun = context.getFun(identifier)
        val args = buildParamValuesByName(fun, arguments, context)
        fun.invoke(args)

      case Some(hostExpr) =>
        val hostValue = hostExpr.calculate(context)
        val extFun = context.getExtFun((hostValue.getClass, identifier))
        val args = buildParamValuesByName(extFun, arguments, context, hostValue)
        extFun.invoke(args)
    }
  }

  // TODO: Call this in syntax checking phase
  /*
  private def buildParametersByName(funDef: FunDef, arguments: List[Expr], namedArguments: List[(Symbol, Expr)], hostExpr: Expr = null): Map[Symbol, Expr] = {
    val paramExprs = if (hostExpr != null) hostExpr::arguments else arguments
    (funDef.parameters zip paramExprs map(infoAndExpr => infoAndExpr._1.name -> infoAndExpr._2 )).toMap
  }
  */

  private def buildParamValuesByName(funDef: FunDef, arguments: List[CallArg], callContext: Context, hostValue: AnyRef = null): Map[Symbol, AnyRef] = {

    var callArgs = Map[Symbol, AnyRef]()

    var pos = if (hostValue != null) 1 else 0 // self is always first parameter
    arguments foreach {arg =>
      val name: Symbol = if (arg.name != null) arg.name else {
        val n = funDef.parameters(pos).name
        pos += 1
        n
      }
      val value: AnyRef = arg.expr.calculate(callContext)

      callArgs += name -> value
    }

    callArgs += CallExpr.SelfParamName -> hostValue

    callArgs
  }
}


case class CallArg(var name: Symbol, expr: Expr)

object CallExpr {
  val SelfParamName = 'self
}