package org.shapefun.functions

import org.shapefun.utils.MathUtils

/**
 *
 */
case class WaveFun(frequency: Double = 1,
                   amplitude: Double = 1,
                   phaseShift: Double = 0) extends Fun {

  override def apply(t: Double): Double = {
    val a = amplitude
    val f = frequency
    val phase = MathUtils.Tau * phaseShift

    math.sin(phase + t * f) * a
  }
}

/**
 *
 */
case class WaveMixFun(startFrequency: Double = 1,
                      endFrequency: Double = 4,
                      startAmplitude: Double = 4,
                      endAmplitude: Double = 1,
                      phaseShift: Double = 0) extends Fun {

  override def apply(t: Double): Double = {
    val amplitude = MathUtils.mix(startAmplitude, endAmplitude, t)
    val frequency = MathUtils.mix(startFrequency, endFrequency, t)
    val phase = MathUtils.Tau * phaseShift

    math.sin(phase + t * frequency) * amplitude
  }
}

