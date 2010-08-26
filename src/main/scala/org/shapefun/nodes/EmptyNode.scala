package org.shapefun.nodes

import org.shapefun.Node

/**
 * 
 */
object EmptyNode extends Node {
  def render() {}
  def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float) {}
}