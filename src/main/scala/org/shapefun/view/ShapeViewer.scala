package org.shapefun.view


import com.jme3.app.SimpleApplication
import com.jme3.system.AppSettings
import com.jme3.asset.plugins.FileLocator
import com.jme3.material.Material
import com.jme3.math.{ColorRGBA, Vector3f}
import com.jme3.asset.AssetManager
import com.jme3.scene.shape.Box
import com.jme3.app.state.ScreenshotAppState
import com.jme3.input.{ChaseCamera, KeyInput}

import org.shapefun.{ModelLoader, Model}
import com.jme3.input.controls.{ActionListener, KeyTrigger}
import org.shapefun.utils.{ModelBuilder, MeshBuilder, FileChangeMonitor, Logging}
import java.io.File
import org.shapefun.parser.{ContextImpl, JavaKind, ShapeLangParser}
import org.shapefun.parser.defs.ExternalFunDef
import org.shapefun.parser.syntaxtree.Expr
import com.jme3.scene.{LightNode, Mesh, Spatial, Geometry}
import org.shapefun.shapes.Plane
import com.jme3.light.DirectionalLight
import org.shapefun.functions._
import org.shapefun.functions.NoiseFun
import org.shapefun.functions.ConstantFun
import org.shapefun.functions.WaveMixFun
import org.shapefun.functions.LinearFun
import org.shapefun.shapes.Plane

/**
 * Live preview of procedural shape defined in a config file.
 * Support for updating the shape when the config file is changed.
 */
object ShapeViewer extends SimpleApplication with Logging {

  private val limitFps = true

  private val startX = 0
  private val startY = 5
  private val startZ = 20

  private var shape: Spatial = null

  private var chaseCamera: ChaseCamera = null


  def main(args: Array[String]) {
    Logging.initializeLogging()

    val settings: AppSettings = new AppSettings(true)
    if (limitFps) {
      settings.setFrameRate(60)
      settings.setVSync(true)
    }
    else {
      settings.setFrameRate(-1)
      settings.setVSync(false)
    }
    setSettings(settings)

    setPauseOnLostFocus(false)

    start()
  }

  def setShape(newShape: Spatial) {
    if (shape != null) {
      shape.removeControl(chaseCamera)
      rootNode.detachChild(shape)
    }

    shape = newShape

    if (shape != null) {
      rootNode.attachChild(shape)
      shape.addControl(chaseCamera)
    }
  }

  def simpleInitApp() {

    assetManager.registerLocator("assets", classOf[FileLocator])

    // getCamera.setFrustumFar(320000)

    // Setup camera control
    chaseCamera = new RotationCameraControl(getCamera, inputManager)
    chaseCamera.setDragToRotate(true)


    // Allow screenshots
    stateManager.attach(new ScreenshotAppState())

    // Background
    viewPort.setBackgroundColor(new ColorRGBA(0.3f, 0.3f, 0.3f, 1f))

    // Sky
    val sky = new Sky(getCamera, assetManager)
    rootNode.attachChild(sky)
    sky.createLights(rootNode)

    // Lights
    val sun = new DirectionalLight()
    sun.setDirection(new Vector3f(1,0,-2).normalizeLocal())
    sun.setColor(ColorRGBA.White)
    rootNode.addLight(sun)

    val antiSun = new DirectionalLight()
    antiSun.setDirection(new Vector3f(-1,1,2).normalizeLocal())
    antiSun.setColor(ColorRGBA.Red)
    rootNode.addLight(antiSun)

    // Start pos
    val startPos: Vector3f = new Vector3f(startX, startY, startZ)
    this.getCamera.setLocation(startPos)


    // Create test model
    val plane = new Plane(
      segments = 12,
      columns = 3,
      xPath = LinearFun(-20, 20),
      yPath = WaveMixFun(endFrequency = 3),
      zPath = NoiseMixFun(endFrequency = 3),
      startProfile = NoiseFun(frequency = 2, amplitude = 0.15, offset = 4233.123),
      endProfile = NoiseFun(frequency = 1.4, amplitude = 0.1, offset = 123.43),
      twist = NoiseFun(frequency = 3.5, amplitude = 0.2),
      scale = LinearFun(10, 1),
      doubleSided = true,
      turnInPathDirection = false )

    val builder = new ModelBuilder()
    plane.buildModel(builder)
    setShape(builder.getModel(assetManager))
  }




}
