package org.shapefun.shapes

import org.shapefun.functions.{LinearFun, ConstantFun, Fun}

/**
 * A continuous plane with some profile, following a path.
 */
case class Plane(var segments: Int = 10,
                var xPath: Fun = LinearFun(),
                var yPath: Fun = ConstantFun(0),
                var zPath: Fun = ConstantFun(0),
                var radius: Fun = ConstantFun(1),
                var startProfile: Fun = LinearFun(0, 1),
                var endProfile: Fun = LinearFun(1, 0),
                var twist: Fun = ConstantFun(0)) extends Shape {



}