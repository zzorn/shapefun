package org.shapefun

import java.util.ArrayList

/**
 * 
 */
class CompositeNode extends Node {

  private val _children = new ArrayList[Node]()

  def add(node: Node) = _children.add(node)
  def remove(node: Node) = _children.remove(node)
  def clear() = _children.clear

  def render() {
    var i = 0
    while (i < _children.size) {
      _children.get(i).render()
      i += 1
    }
  }


  def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float) {
    var i = 0
    while (i < _children.size) {
      _children.get(i).update(secondsSinceLastUpdate, secondsSinceStart)
      i += 1
    }
  }

}