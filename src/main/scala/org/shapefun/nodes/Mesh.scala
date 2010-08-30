package org.shapefun

import util.Random

import org.lwjgl.opengl.GL11._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{ARBBufferObject, ARBVertexBufferObject, GLContext}

import simplex3d.math._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._ 

import simplex3d.buffer._
import simplex3d.buffer.floatm._

import java.nio.IntBuffer


/**
 * 
 */
class Mesh extends Node {

  private var vertexCount = 0
  private var indexCount = 0
  private var nextFreeIndex = 0
  private var nextFreeVertex = 0
  private var vertexes: DataArray[Vec3, RawFloat] = null
  private var normals: DataArray[Vec3, RawFloat] = null
  private var textureCoordinates: DataArray[Vec2, RawFloat] = null
  private var colors: DataArray[Vec4, RawFloat] = null
  private var indexes: DataArray[Int1, SInt] = null

  var texture: TexturePart = null

  // TODO: Check for max vertex / index when adding

  def buildMesh() {} // TODO: Do we need?

  def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float) {}

  def allocateBuffers(newVertexCount: Int, newIndexCount: Int) {
    if (vertexCount != newVertexCount) {
      vertexCount = newVertexCount
      vertexes = DataArray[Vec3, RawFloat](vertexCount)
      normals = DataArray[Vec3, RawFloat](vertexCount)
      textureCoordinates = DataArray[Vec2, RawFloat](vertexCount)
      colors = DataArray[Vec4, RawFloat](vertexCount)
    }

    if (indexCount != newIndexCount) {
      indexCount = newIndexCount
      indexes = DataArray[Int1, SInt](indexCount)
    }

    clear()
  }


  def clear() {
    nextFreeVertex = 0
    nextFreeIndex = 0
  }

  def render() {

/*
    if(GLContext.getCapabilities().GL_ARB_vertex_buffer_object) renderWithVbo()
    else renderWithDirectMode()
*/
    renderWithDirectMode()
  }

  private def renderWithVbo() {
    def createVboId: Int = {
      val buffer: IntBuffer = BufferUtils.createIntBuffer(1)
      ARBBufferObject.glGenBuffersARB(buffer);
      buffer.get(0);
    }

    val id: Int = createVboId

    // TODO: Use an interleaved Vertex Buffer Object

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    ARBBufferObject.glBindBufferARB(ARBVertexBufferObject.GL_ARRAY_BUFFER_ARB, id);

    val stride = (3 + 3 + 4 + 2) * 4 // 3 for vertex, 3 for normal, 4 for colour and 2 for texture coordinates. * 4 for bytes

    // vertices
    var offset = 0 * 4; // 0 as its the first in the chunk, i.e. no offset. * 4 to convert to bytes.
    glVertexPointer(3, GL_FLOAT, stride, offset);

    // normals
    offset = 3 * 4; // 3 components is the initial offset from 0, then convert to bytes
    glNormalPointer(GL_FLOAT, stride, offset);

    // colours
    offset = (3 + 3) * 4; // (6*4) is the number of byte to skip to get to the colour chunk
    glColorPointer(4, GL_FLOAT, stride, offset);

    // texture coordinates
    offset = (3 + 3 + 2) * 4;
    glTexCoordPointer(2, GL_FLOAT, stride, offset);


  }

  private def renderWithDirectMode() {
//    if (texture != null) texture.texture.bind()
    println("renderWithDirectMode called")

    glBegin(GL_TRIANGLES)

    var i: Int = 0
    while (i < indexes.size) {
      val index: Int = indexes(i)

      val tex: Vec2 = textureCoordinates(index)
      val color: Vec4 = colors(index)
      val pos: Vec3 = vertexes(index)
      val normal: Vec3 = normals(index)

      glTexCoord2d(tex.x, tex.y)
      glColor4d(color.r, color.g, color.b, color.a)
      glNormal3d(normal.x, normal.y, normal.z)
      glVertex3d(pos.x, pos.y, pos.z)

      i += 1
    }

    glEnd()

  }


  /* Adds a vertex and returns its ordinal number. */
  def addVertex(pos: inVec3, normal: inVec3 = Vec3.UnitY, textureCoordinate: inVec2 = Vec2.Zero, color: inVec4 = Vec4(1,1,1,1)): Int = {
    val index = nextVertex()
    vertexes(index) = pos
    normals(index) = normal
    textureCoordinates(index) = textureCoordinate
    colors(index) = color
    index
  }

  def setVertex(index: Int, pos: inVec3, normal: inVec3 = null, textureCoordinate: inVec2 = null, color: inVec4 = null) {
    if (pos != null) vertexes(index) = pos
    if (normal != null) normals(index) = normal
    if (textureCoordinate != null) textureCoordinates(index) = textureCoordinate
    if (color != null) colors(index) = color
  }

  def setNormal(index: Int, normal: inVec3) {
    normals(index) = normal
  }

  def setTextureCoordinate(index: Int, textureCoordinate: inVec2) {
    textureCoordinates(index) = textureCoordinate
  }

  def setColor(index: Int, color: inVec4) {
    colors(index) = color
  }

  def addIndex(i: Int) {
    val index = nextIndex()
    indexes(index) =  i
    index
  }

  def addTriangle(a: Int, b: Int, c: Int) {
    addIndex(a)
    addIndex(b)
    addIndex(c)
  }

  def addQuad(a: Int, b: Int, c: Int, d: Int) {
    addTriangle(a, b, c)
    addTriangle(c, d, a)
  }

  def smoothAllNormals() {
    // TODO: Implement

    // Clear normals table (set to zero)

    // Create a number of sides array

    // Loop through all triangles

    // Calculate the normal for the triangle
    // Add normal to triangle corner vertex normals
    // Increase the number of sides counter for the corners


    // Loop through all normals, divide them with number of sides, if larger than 0
  }


  def randomizeColors() {
    val rand = new Random()
    val max = colors.size
    var i = 0
    while (i < max) {
      colors(i) = Vec4(rand.nextFloat, rand.nextFloat, rand.nextFloat,1 )
      i += 1
    }
  }

  private def nextVertex(): Int = {
    val current = nextFreeVertex
    nextFreeVertex += 1
    current
  }

  private def nextIndex(): Int = {
    val current = nextFreeIndex
    nextFreeIndex += 1
    current
  }

}