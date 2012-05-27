package org.shapefun.parser.syntaxtree

import org.shapefun.parser.{BoolKind, NumKind, Kind, Context}


/**
 *
 */
case class BooleanOp(a: Expr, op1: Symbol, b: Expr) extends Expr {

  val allowedOps = Set('and, 'or, 'xor)

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    ensureExprIsAssignableTo(BoolKind, a, staticContext)
    ensureExprIsAssignableTo(BoolKind, b, staticContext)

    if (!allowedOps.contains(op1)) reportTypeError("Operator '"+op1.name+"' is not allowed", this, staticContext)

    BoolKind
  }

  def calculate(context: Context): AnyRef = {
    val aVal = Boolean.unbox(a.calculate(context))
    val bVal = Boolean.unbox(b.calculate(context))

    Boolean.box(compare(aVal, op1, bVal))
  }

  private def compare(v1: Boolean, op: Symbol, v2: Boolean): Boolean = {
    op match {
      case 'and =>  v1 && v2
      case 'or  =>  v1 || v2
      case 'xor =>  (!v1 && v2) || (v1 && !v2)
    }
  }

}