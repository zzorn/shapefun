package org.shapefun.examples

import org.shapefun.Screen
import org.shapefun.models.Cube

/**
 * Shows a single rotating cube in 3D
 */
object RotatingCubeExample extends Screen {
  val cube = new Cube()
//  cube.rotation animate Rotate() // Rotate takes as arguments rotation axis and speed
  add(cube)

}