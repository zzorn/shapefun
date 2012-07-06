package org.shapefun.functions

/**
 * A function from the range 0..1 to some arbitrary values.
 */
trait Fun extends ((Double) => Double) {

  /**
   * @param t a parameter between 0 and 1, inclusive.
   * @return the value for the specified value of t.
   */
  def apply(t: Double): Double

}