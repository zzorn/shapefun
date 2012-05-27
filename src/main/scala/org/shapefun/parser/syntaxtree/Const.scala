package org.shapefun.parser.syntaxtree

import org.shapefun.parser._


/**
 *
 */
case class Const(value: AnyRef) extends Expr{

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    if (value == null) reportTypeError("value of constant was null", this, staticContext)

    if (value == Unit) UnitKind
    else if (classOf[Double].isInstance(value)) NumKind
    else if (classOf[Boolean].isInstance(value)) BoolKind
    else JavaKind(value.getClass)
  }

  def calculate(context: Context) = value
}