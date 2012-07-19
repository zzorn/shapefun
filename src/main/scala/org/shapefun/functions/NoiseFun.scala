package org.shapefun.functions

import org.shapefun.utils.{NoiseUtils, MathUtils, SimplexGradientNoise}

/**
 *
 */
case class NoiseMixFun(var startFrequency: Double = 1,
                       var endFrequency: Double = 1,
                       var startAmplitude: Double = 1,
                       var endAmplitude: Double = 1,
                       var startOctaves: Double = 1,
                       var endOctaves: Double = 1,
                       var offset: Double = 0) extends Fun {

  override def apply(t: Double): Double = {
    NoiseUtils.turbulence1(
      t + offset,
      MathUtils.mix(startFrequency, endFrequency, t),
      MathUtils.mix(startAmplitude, endAmplitude, t),
      MathUtils.mix(startOctaves, endOctaves, t))
  }
}

/**
 *
 */
case class NoiseFun(var frequency: Double = 1,
                    var amplitude: Double = 1,
                    var octaves: Double = 1,
                    var offset: Double = 0) extends Fun {

  override def apply(t: Double): Double = {
    NoiseUtils.turbulence1(t + offset, frequency, amplitude, octaves)
  }
}