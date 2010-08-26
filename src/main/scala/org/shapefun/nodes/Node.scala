package org.shapefun

import simplex3d.math._ // Using primitive casting.
import simplex3d.math.floatm.renamed._ // Using short names for Double data types.
import simplex3d.math.floatm.FloatMath._ // Using all double functions.

/**
 * 
 */

trait Node {

  val transformation = Mat4.apply(1)

  // TODO: Transformation matrix, function for changing pos, rotation, scale, etc

  // TODO: Bounding sphere (or box?)
  // TODO: Keep track of parent?

  /**
   * Update any animations, mesh construction, etc
   * @param secondsSinceLastUpdate number of seconds since update was called last, or since the game was initialized if this is the first call.
   * @param secondsSinceStart number of seconds since the application started.
   */
  def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float)

  /**
   * Render the node with opengl calls.
   */
  def render()

}