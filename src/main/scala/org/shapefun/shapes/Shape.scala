package org.shapefun.shapes

import com.jme3.scene.Spatial
import org.shapefun.utils.ModelBuilder
import com.jme3.math.{Transform, Vector3f}

/**
 *
 */
trait Shape {

  var transformation: Transform = new Transform()

  def buildModel(modelBuilder: ModelBuilder) {
    // TODO: Implement in each shape.
  }

}