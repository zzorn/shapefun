package org.shapefun.utils

object MathUtils {

  /**
   * 2 Pi, that is, one turn.  See http://tauday.com/
   */
  val Tau = 2 * math.Pi

  val TauFloat = Tau.toFloat

  /**
   * @return linear interpolation of a and b, using t.
   */
  def mix(a: Double, b: Double, t: Double): Double = {
    a * (1.0 - t) + b * t
  }

  /**
   * @return linear mapping of value from source range to destination range.
   */
  def map(value: Double, sourceStart: Double, sourceEnd: Double, destinationStart: Double, destinationEnd: Double): Double = {
    mix(destinationStart, destinationEnd, relativePos(value, sourceStart, sourceEnd))
  }

  /**
   * @return 0 if value is start, 1 if value is end, and a relative position between them otherwise.
   */
  def relativePos(value: Double, start: Double, end: Double): Double = {
    if (start == end) 0.5
    else (value - start) / (end - start)
  }


  /**
   * Hermite interpolation between two values.
   * NOTE: Assumes even spacing between values.
   * TODO: Take x positions of points, and use them to calculate the line coefficients m0 and m1
   *
   * Based on code from http://paulbourke.net/miscellaneous/interpolation/
   *
   * @param prev the value before start
   * @param start the start value
   * @param end the end value
   * @param next the value after end
   * @param t the relative position between start and end (0 = start, 1 = end)
   * @param tension 1 is high, 0 normal, -1 is low
   * @param bias 0 is even, positive is towards first segment, negative towards second.
   * @return an interpolation between points start and end, given the points prev and next, and a relative position t (0..1) between start and end.
   */
  def hermiteInterpolation (prev: Double,
                            start: Double,
                            end: Double,
                            next: Double,
                            t: Double,
                            tension: Double = 0,
                            bias: Double = 0): Double = {

    // Calculate t^2 and t^3
    val t2 = t * t
    val t3 = t * t2

    // Calculate line coefficients
    val m0  = (start - prev ) * (1 + bias) * (1 - tension ) / 2 +
              (end   - start) * (1 - bias) * (1 - tension ) / 2
    val m1  = (end   - start) * (1 + bias) * (1 - tension ) / 2 +
              (next  - end  ) * (1 - bias) * (1 - tension ) / 2

    // Magic happens here
    val a0 =  2 * t3 - 3 * t2 + 1
    val a1 =      t3 - 2 * t2 + t
    val a2 =      t3 -     t2
    val a3 = -2 * t3 + 3 * t2

    // More magic
    a0 * start +
    a1 * m0 +
    a2 * m1 +
    a3 * end
  }



}