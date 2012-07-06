package org.shapefun.functions

import org.shapefun.utils.MathUtils

/**
 *
 */
case class LinearFun(startValue: Double = 0,
                     endValue: Double = 1) extends Fun {

  override def apply(t: Double) = MathUtils.mix(startValue, endValue, t)
}