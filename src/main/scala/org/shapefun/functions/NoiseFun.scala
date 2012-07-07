package org.shapefun.functions

import org.shapefun.utils.{MathUtils, SimplexGradientNoise}

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
    var value = 0.0
    var f = MathUtils.mix(startFrequency, endFrequency, t)
    var a = MathUtils.mix(startAmplitude, endAmplitude, t)
    var o = math.min(MathUtils.mix(startOctaves, endOctaves, t), 10.0) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise1(offset + t * f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
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
    var value = 0.0
    var f = frequency
    var a = amplitude
    var o = math.min(octaves, 10.0) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise1(offset + t * f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
  }
}