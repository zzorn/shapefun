package org.shapefun.view


import com.jme3.app.SimpleApplication
import com.jme3.system.AppSettings
import com.jme3.asset.plugins.FileLocator
import com.jme3.material.Material
import com.jme3.math.{ColorRGBA, Vector3f}
import com.jme3.asset.AssetManager
import com.jme3.scene.shape.Box
import com.jme3.app.state.ScreenshotAppState
import com.jme3.input.KeyInput

import org.shapefun.{ModelLoader, Model}
import com.jme3.input.controls.{ActionListener, KeyTrigger}
import org.shapefun.utils.{MeshBuilder, FileChangeMonitor, Logging}
import java.io.File
import org.shapefun.parser.{ContextImpl, JavaKind, ShapeLangParser}
import org.shapefun.parser.defs.ExternalFunDef
import org.shapefun.parser.syntaxtree.Expr
import com.jme3.scene.{Mesh, Spatial, Geometry}

/**
 * Live preview of procedural shape defined in a config file.
 * Support for updating the shape when the config file is changed.
 */
object ShapeViewer extends SimpleApplication with Logging {

  private val wireframe = false
  private val limitFps = true

  private val startX = 0
  private val startY = 5
  private val startZ = 20

  private var shape: Spatial = null

  private var model: Model = null

  private var tomes: Map[String, Model] = Map()

  private var chaseCamera: RotationCameraControl = null

  private var modelLoader: ModelLoader = null
  private val modelSource: File = new File("assets/shapes/TestShape.funlang")
  private var modelSourcesToRead: File = null
  private var fileChangeMonitor: FileChangeMonitor = null

  private val parser = new ShapeLangParser(Map('Mesh -> JavaKind(classOf[MeshBuilder]) ))


  def main(args: Array[String]) {
    Logging.initializeLogging()

    modelLoader = new ModelLoader()

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

    // Start polling
    fileChangeMonitor = new FileChangeMonitor(modelSource, setFileToRead _)
    fileChangeMonitor.start()

    start()
  }

  private def setFileToRead(file: File) {
    synchronized {
      modelSourcesToRead = file
    }
  }

  /**
   * @return changed file, null if not changed.
   */
  private def getFileToRead: File = {
    synchronized {
      val file = modelSourcesToRead
      modelSourcesToRead = null
      file
    }
  }

  def updateModel(file: File) {
    if (shape != null) {
      shape.removeControl(chaseCamera)
      rootNode.detachChild(shape)
    }

    shape = createModel(assetManager, file)

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

    // Start pos
    val startPos: Vector3f = new Vector3f(startX, startY, startZ)
    this.getCamera.setLocation(startPos)

    // Load model
    //updateModel()

    // TODO: Wireframe support
    // val terrainMaterial = if (wireframe) createWireframeMaterial(assetManager) else createSimpleTerrainMaterial(getAssetManager)


    setupKeys()


  }


  override def simpleUpdate(tpf: Float) {
    val file = getFileToRead
    if (file != null) {
      updateModel(file)
    }


  }

  private def createModel(assetManager: AssetManager, modelFile: File): Spatial = {
    println("create model")

    println("  create context")
    val rootContext: ContextImpl = new ContextImpl()

    println("  Add mesh constr")
    rootContext.addFun0('Mesh) { () => new MeshBuilder() }
    println("  Add rand color")
    rootContext.addFun0('randomColor) { () => ColorRGBA.randomColor() }
    println("  Add rand vec")
    rootContext.addFun3('Vec) { (x: java.lang.Double, y: java.lang.Double, z: java.lang.Double) =>
      new Vector3f(
        Double.unbox(x).toFloat,
        Double.unbox(y).toFloat,
        Double.unbox(z).toFloat
      )
    }

    println("  Add geom")
    rootContext.addFun2('Geom) { (mesh: MeshBuilder, color: ColorRGBA) =>
      println("addGeom")

      val createdMesh: Mesh = mesh.createMesh()

      val mat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md")
      mat.setColor("Color", color)

      val geometry: Geometry = new Geometry("mesh", createdMesh)
      geometry.setMaterial(mat)
      geometry
    }

    rootContext.addExtFun1(classOf[MeshBuilder], 'addVertex) { (m: MeshBuilder, pos: Vector3f) =>
      println("addVertex")
      Integer.valueOf(m.addVertex(pos))
    }

    rootContext.addExtFun4(classOf[MeshBuilder], 'addQuad) { (m: MeshBuilder,
                                        i0: java.lang.Integer,
                                        i1: java.lang.Integer,
                                        i2: java.lang.Integer,
                                        i3: java.lang.Integer) =>
      println("addQuad")
      m.addQuad(i0, i1, i2, i3)
      m
    }

    println("  Parse file")
    val expr: Expr = parser.parseFile(modelFile, rootContext)

    println("  Parse calculate")
    val result: AnyRef = expr.calculate(rootContext)

    println("  ready")
    result.asInstanceOf[Spatial]

    /*
    model = loadModel(modelFile)
    if (model != null) model.createSpatial(assetManager)
    else null*/
  }

  private def loadModel(modelFile: File): Model = {
    /*
    val newTomes: Map[String, Model] = modelLoader.loadModels(modelFile)
    tomes ++= newTomes

    if (newTomes.isEmpty) null
    else newTomes.head._2

    // TODO: Detect removed tomes

    */
    //new Cube()
    null
  }

  private def setupKeys() {
    inputManager.addMapping("ReloadModel", new KeyTrigger(KeyInput.KEY_SPACE))

    inputManager.addListener(new ActionListener {
      def onAction(name: String, isPressed: Boolean, tpf: Float) { if (!isPressed) updateModel(modelSource) }
    }, "ReloadModel")
  }


  private def makeTestBox(pos: Vector3f = Vector3f.ZERO): Geometry = {
    val box = new Geometry("box", new Box(1, 1, 1))
    box.setLocalTranslation(pos)
    val mat = new Material(assetManager, "Common/MatDefs/Misc/Unshaded.j3md")
    mat.setColor("Color", ColorRGBA.randomColor())
    box.setMaterial(mat)
    box
  }


}
