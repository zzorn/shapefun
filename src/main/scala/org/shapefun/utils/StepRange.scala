package org.shapefun.utils

object StepRange {

  def withSteps(start: Double, end: Double, stepsCount: Double, inclusive: Boolean = false): StepRange = {
    val step: Double = (math.abs(end - start) + (if (inclusive) 1 else 0)) / stepsCount
    StepRange(start, end, step, inclusive)
  }

}

/**
 *
 */
case class StepRange(start: Double, end: Double, step: Double = 1, inclusive: Boolean = false) {

  /**
   * @return value to add to start some specified times to get to the end.
   *         Same absolute value as step, but with a negative sign if end is smaller than start.
   */
  def stepIncrement = if (start < end) step else -step

}