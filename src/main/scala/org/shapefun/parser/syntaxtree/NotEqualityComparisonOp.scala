package org.shapefun.parser.syntaxtree

import org.shapefun.parser._


/**
 *
 */
case class NotEqualityComparisonOp(a: Expr, b: Expr) extends Expr {

  private var useNumCompare = false

  protected def doCalculateTypes(staticContext: StaticContext): Kind = {
    val k1 = ensureExprIsAssignableTo(AnyRefKind, a, staticContext)
    val k2 = ensureExprIsAssignableTo(AnyRefKind, b, staticContext)

    useNumCompare = NumKind == k1 &&
                    NumKind == k2

    BoolKind
  }

  def calculate(context: Context): AnyRef = {
    val v1 = a.calculate(context)
    val v2 = b.calculate(context)

    if (useNumCompare) {
      // For numbers, perform equality matching using epsilon, to account for drift-off and rounding errors in double calculations
      val n1 = Double.unbox(v1)
      val n2 = Double.unbox(v2)
      Boolean.box(n1 < n2 - Num.Epsilon ||
                  n1 > n2 + Num.Epsilon)
    }
    else {
      Boolean.box(v1 != v2)
    }
  }


}
