package org.shapefun.functions

/**
 *
 */
case class ConstantFun(value: Double = 0) extends Fun {

  override def apply(t: Double) = value
}