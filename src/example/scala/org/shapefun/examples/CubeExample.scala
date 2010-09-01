package org.shapefun.examples

import org.shapefun.Screen
import org.shapefun.models.Cube

/**
 * Shows a single static 3D cube.
 */
object CubeExample extends Screen {

  add(new Cube())

  /*
  for (x <- -3 to 3)
    for (y <- -3 to 3)
     for (z <- -3 to 3)
      add(new Cube(100 * x, 100 * y, 100 * z))
  */
}
