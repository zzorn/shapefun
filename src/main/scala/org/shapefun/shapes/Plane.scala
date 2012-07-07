package org.shapefun.shapes

import org.shapefun.functions.{LinearFun, ConstantFun, Fun}
import org.shapefun.utils.{MathUtils, ModelBuilder}
import com.jme3.math._
import org.shapefun.functions.ConstantFun
import org.shapefun.functions.LinearFun
import MathUtils._

/**
 * A continuous plane with some profile, following a path.
 */
// TODO: Extract movement along path into utility code - same code is used for tubes.
case class Plane(var segments: Int = 10,
                 var columns: Int = 4,
                 var xPath: Fun = LinearFun(0, 10),
                 var yPath: Fun = ConstantFun(0),
                 var zPath: Fun = ConstantFun(0),
                 var scale: Fun = ConstantFun(1),
                 var startProfile: Fun = LinearFun(0, 0.5),
                 var endProfile: Fun = LinearFun(0.5, 0),
                 var twist: Fun = ConstantFun(0),
                 var doubleSided: Boolean = true,
                 var turnInPathDirection: Boolean = false) extends Shape {

  override def buildModel(modelBuilder: ModelBuilder) {
    val meshBuilder = modelBuilder.meshBuilder

    meshBuilder.pushTransform(transformation)

    val center = new Vector3f()
    val prevCenter = new Vector3f()
    val nextCenter = new Vector3f()

    val direction = new Quaternion()
    direction.fromAngles(0, 0, 0)

    val twistRot = new Quaternion()

    val oldLookDirection = new Vector3f(-1, 0, 0)
    val lookDirection = new Vector3f(-1, 0, 0)
    val upDirection = new Vector3f(0, 1, 0)
    val twistAxle = new Vector3f(0, 0, 1)

    val segmentTransformation = new Matrix4f()

    // Loop over segments and columns
    var seg = 0
    while (seg < segments) {
      val relNextSeg = MathUtils.relativePos(seg + 1, 0, segments - 1)
      val relSeg = MathUtils.relativePos(seg, 0, segments - 1)

      prevCenter.set(center)

      if (seg == 0) {
        // Handle first segment
        val relPrevSeg = MathUtils.relativePos(seg + 1, 0, segments - 1)

        prevCenter.set(xPath(relPrevSeg).toFloat,
                       yPath(relPrevSeg).toFloat,
                       zPath(relPrevSeg).toFloat)

        center.set(xPath(relSeg).toFloat,
                   yPath(relSeg).toFloat,
                   zPath(relSeg).toFloat)

      }
      else {
        // Normal case
        prevCenter.set(center)
        center.set(nextCenter)
      }

      // Calculate center position at next segment
      nextCenter.set(xPath(relNextSeg).toFloat,
                     yPath(relNextSeg).toFloat,
                     zPath(relNextSeg).toFloat)



      // Calculate direction at segment
      if (turnInPathDirection) {
        lookDirection.set(prevCenter).subtractLocal(nextCenter).normalizeLocal()
        if (lookDirection.lengthSquared() == 0) {
          // If the moved distance is zero, use the previous direction
          lookDirection.set(oldLookDirection)
        }
        else {
          oldLookDirection.set(lookDirection)
        }
        direction.lookAt(lookDirection, upDirection)
      }
      else {
        direction.fromAngles(0, -TauFloat * 0.25f, 0)
      }

      // Turn direction by twist
      val twistAngle = (twist.apply(relSeg) * MathUtils.Tau).toFloat
      twistRot.fromAngleNormalAxis(twistAngle, twistAxle)
      direction.multLocal(twistRot)

      // Calculate scaling
      val segmentScale = scale.apply(relSeg).toFloat

      // Calculate transformation
      segmentTransformation.loadIdentity()
      segmentTransformation.setTranslation(center)
      segmentTransformation.setRotationQuaternion(direction)
      meshBuilder.pushTransform(segmentTransformation)

      var col = 0
      while (col < columns) {
        val relCol = MathUtils.relativePos(col, 0, columns - 1)

        // Create vertex
        val startP = startProfile.apply(relCol)
        val endP = endProfile.apply(relCol)

        val y = segmentScale * MathUtils.mix(startP, endP, relSeg).toFloat
        val x = segmentScale * (2f * relCol.toFloat - 1f)
        val z = 0

        val pos = new Vector3f(x, y, z)
        val tex = new Vector2f(relCol.toFloat, relSeg.toFloat)
        val color = ColorRGBA.randomColor()
        val index = meshBuilder.addVertex(pos, null, tex, color)

        // Stitch faces
        val i0 = index - columns - 1
        val i1 = index - columns
        val i2 = index
        val i3 = index - 1
        if (seg > 0 && col > 0) {
          meshBuilder.addQuad(i0, i1, i2, i3, doubleSided)
        }

        col += 1
      }

      meshBuilder.popTransform()
      seg += 1
    }



    meshBuilder.popTransform()
  }

}