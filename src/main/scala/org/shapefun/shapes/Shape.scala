package org.shapefun.shapes

import com.jme3.scene.Spatial
import org.shapefun.utils.ModelBuilder
import com.jme3.math.{Matrix4f, Transform, Vector3f}

/**
 *
 */
trait Shape {

  var transformation: Matrix4f = new Matrix4f()

  def buildModel(modelBuilder: ModelBuilder) {
    // TODO: Implement in each shape.
  }

  def setPosition(pos: Vector3f) {
    transformation.setTranslation(pos)
  }

  def getPosition: Vector3f = {
    transformation.toTranslationVector
  }

}