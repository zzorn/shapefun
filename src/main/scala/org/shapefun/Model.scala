package org.shapefun

import com.jme3.material.Material
import com.jme3.asset.AssetManager
import com.jme3.math.Matrix4f
import utils.MeshBuilder
import com.jme3.scene.{Mesh, Geometry, Spatial}


/**
 *
 */

trait Model {


  def createSpatial(assetManager: AssetManager): Spatial = {
    val mesh = createMesh()
    val geometry: Geometry = new Geometry("shape", mesh)
    geometry.setMaterial(createMaterial(assetManager))
    geometry
  }

  def buildMesh(builder: MeshBuilder)

  def buildMesh(builder: MeshBuilder, transformation: Matrix4f) {
    builder.pushTransform(transformation)
    buildMesh(builder)
    builder.popTransform()
  }

  def createMesh(): Mesh = {
    val builder = new MeshBuilder()
    buildMesh(builder)
    builder.createMesh()
  }

  def createMaterial(assetManager: AssetManager): Material = {
    val mat = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md");
    //val mat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md");
    //mat.setColor("Color", ColorRGBA.randomColor());

    mat
  }

}
