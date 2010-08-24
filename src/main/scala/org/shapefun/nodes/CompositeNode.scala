package org.shapefun

import java.util.ArrayList

/**
 * 
 */
class CompositeNode extends Node {

  private var _children = new ArrayList[Node]()

  def add(node: Node) = _children.add(node)
  def remove(node: Node) = _children.remove(node)
  def clear() = _children.clear

  def render() {
    var i = 0
    while (i < _children.size) {
      _children.get(i).render
    }
  }

  def update() {
    var i = 0
    while (i < _children.size) {
      _children.get(i).update
    }
  }

}