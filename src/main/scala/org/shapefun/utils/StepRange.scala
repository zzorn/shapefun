package org.shapefun.utils

object StepRange {

  def withSteps(start: Double, end: Double, stepsCount: Double, inclusive: Boolean = false): StepRange = {
    StepRange(start, end, math.abs(end - start) / stepsCount, inclusive)
  }

}

/**
 *
 */
case class StepRange(start: Double, end: Double, step: Double = 1, inclusive: Boolean = false) {

  def stepIncrement = if (start < end) step else -step

}