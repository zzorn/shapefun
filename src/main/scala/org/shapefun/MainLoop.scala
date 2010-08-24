package org.shapefun

import nodes.EmptyNode

/**
 * 
 */
trait MainLoop {

  var root: Node = EmptyNode

  def init

  def update

  def render
  
}