package org.shapefun.utils

import com.jme3.scene.{Geometry, Spatial}
import com.jme3.material.Material
import com.jme3.asset.AssetManager
import com.jme3.math.ColorRGBA
import com.jme3.material.RenderState.BlendMode

/**
 * Allows building a model that can consists of several meshes with different textures and other parameters assigned to each mesh.
 */
class ModelBuilder {

  // TODO: Support multiple meshes with different textures

  val meshBuilder: MeshBuilder = new MeshBuilder

  def getModel(assetManager: AssetManager): Spatial = {
    val mat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md")
    mat.setTexture("ColorMap", assetManager.loadTexture("textures/placeholder2.png"))
    //mat.getAdditionalRenderState.setBlendMode(BlendMode.Alpha)
    mat.getAdditionalRenderState.setAlphaTest(true)
    mat.getAdditionalRenderState.setAlphaFallOff(0.75f)
        /*
    val mat = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md")
    mat.setTexture("DiffuseMap", assetManager.loadTexture("textures/placeholder2.png"))
    mat.setTexture("SpecularMap", assetManager.loadTexture("textures/placeholder.png"))
    mat.setColor("Diffuse", ColorRGBA.randomColor())
    mat.setColor("Ambient", ColorRGBA.randomColor())
    mat.setColor("Specular", ColorRGBA.randomColor())
    mat.setBoolean("UseMaterialColors", true)
    */

    val mesh = meshBuilder.createMesh(calculateNormals = true)
    val geometry = new Geometry("mesh", mesh)
    geometry.setMaterial(mat)

    geometry
  }

}