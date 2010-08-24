package org.shapefun

/**
 * 
 */

trait Node {

  // TODO: Transformation matrix, function for changing pos, rotation, scale, etc
  // TODO: Bounding sphere (or box?)
  // TODO: Keep track of parent?

  /**
   * For animation, mesh construction, etc
   */
  // TODO: Pass in time since last call, and gametime / simulated time, and maybe real time
  def update()

  /**
   * For rendering the node with opengl calls.
   */
  // TODO: Pass in opengl object, and maybe screen
  def render()
}