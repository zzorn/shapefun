package org.shapefun.models

import org.shapefun.Mesh

import simplex3d.math._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._

/**
 * 
 */
case class Cube extends Mesh {

  // TODO: Eliminate the need to know in advance the number of vertexes and indexes
  allocateBuffers(8, 3*2*6)

  addVertex(Vec3(-1,-1,-1))
  addVertex(Vec3(-1,-1, 1))
  addVertex(Vec3( 1,-1, 1))
  addVertex(Vec3( 1,-1,-1))

  addVertex(Vec3(-1, 1,-1))
  addVertex(Vec3(-1, 1, 1))
  addVertex(Vec3( 1, 1, 1))
  addVertex(Vec3( 1, 1,-1))

  addQuad(0, 1, 2, 3)
  addQuad(7, 6, 5, 4)

  addQuad(0, 7, 6, 1)
  addQuad(2, 5, 4, 3)

  addQuad(1, 6, 5, 2)
  addQuad(3, 4, 7, 0)

}