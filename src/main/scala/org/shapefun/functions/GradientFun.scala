package org.shapefun.functions

import java.util.TreeMap
import java.util.Map.Entry
import org.shapefun.utils.MathUtils


/**
 * User defined function shape.
 */
case class GradientFun(var guidePoints: TreeMap[Double, Double] = new TreeMap[Double, Double]()) extends Fun {

  override def apply(t: Double): Double = {
    val floorEntry = guidePoints.floorEntry(t)
    val ceilingEntry = guidePoints.ceilingEntry(t)

    if (floorEntry == null && ceilingEntry == null) 0
    else if (floorEntry == null) ceilingEntry.getValue
    else if (ceilingEntry == null) floorEntry.getValue
    else {
      // Get previous and next values as well, for smooth continuous interpolation
      var previousEntry = guidePoints.lowerEntry(floorEntry.getValue)
      var nextEntry     = guidePoints.higherEntry(ceilingEntry.getValue)

      // Use same value at endpoints
      if (previousEntry == null) previousEntry = floorEntry
      if (nextEntry     == null) nextEntry     = ceilingEntry

      // Calculate relative position between floor and ceiling entry
      val pos = MathUtils.map(t, floorEntry.getValue, ceilingEntry.getValue, 0, 1)

      // Interpolate with previous and next entry to get current value
      MathUtils.hermiteInterpolation(
        previousEntry.getValue,
        floorEntry.getValue,
        ceilingEntry.getValue,
        nextEntry.getValue,
        pos
      )
    }

  }
}