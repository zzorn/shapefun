package org.shapefun

import simplex3d.math.floatm.FloatMath._
import simplex3d.math.floatm.{Quat4f, Vec3f}
import org.lwjgl.opengl.GL11._

/**
 * 
 */
class Camera extends Node {

  val position = Vec3f(0, 0, -100)
  val direction = Quat4f(Quat4f.Identity)


  def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float) {}


  def render() {}
}
