package org.shapefun.utils

/**
 * Noise related functions.  Uses SimplexGradientNoise.
 */
object NoiseUtils {

  var MaxOctaves = 10.0

  def turbulence1(x: Double,
                  frequency: Double = 1,
                  amplitude: Double = 1,
                  octaves: Double = 1): Double = {
    var value = 0.0
    var f = frequency
    var a = amplitude
    var o = math.min(octaves, MaxOctaves) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise1(x * f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
  }

  def turbulence2(x: Double,
                  y: Double,
                  frequency: Double = 1,
                  amplitude: Double = 1,
                  octaves: Double = 1): Double = {
    var value = 0.0
    var f = frequency
    var a = amplitude
    var o = math.min(octaves, MaxOctaves) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise2(x*f, y*f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
  }

  def turbulence3(x: Double,
                  y: Double,
                  z: Double,
                  frequency: Double = 1,
                  amplitude: Double = 1,
                  octaves: Double = 1): Double = {
    var value = 0.0
    var f = frequency
    var a = amplitude
    var o = math.min(octaves, MaxOctaves) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise3(x*f, y*f, z*f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
  }

  def turbulence4(x: Double,
                  y: Double,
                  z: Double,
                  w: Double,
                  frequency: Double = 1,
                  amplitude: Double = 1,
                  octaves: Double = 1): Double = {
    var value = 0.0
    var f = frequency
    var a = amplitude
    var o = math.min(octaves, MaxOctaves) // limit max number of octaves
    while (o > 0) {

      // If octaves is a fraction, scale the amplitude of the last octave by the fraction that it is included.
      if (o < 1) a *= o

      // Add noise
      value += SimplexGradientNoise.sdnoise4(x*f, y*f, z*f, w*f) * a

      // Move to smaller details
      f *= 2.0
      a *= 0.5
      o -= 1.0
    }

    value
  }

}