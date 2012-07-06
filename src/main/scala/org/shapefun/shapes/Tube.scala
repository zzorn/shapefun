package org.shapefun.shapes

import org.shapefun.functions.{LinearFun, ConstantFun, Fun}

/**
 * A tube shape, taking a path specified by float functions.
 */
case class Tube(var segments: Int = 10,
                var xPath: Fun = ConstantFun(0),
                var yPath: Fun = LinearFun(),
                var zPath: Fun = ConstantFun(0),
                var radius: Fun = ConstantFun(1),
                var twist: Fun = ConstantFun(0),
                var startProfile: Fun = ConstantFun(1),
                var endProfile: Fun = ConstantFun(1),
                var closeStart: Boolean = true,
                var closeEnd: Boolean = true)  extends Shape {

}