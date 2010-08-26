package org.shapefun

import nodes.EmptyNode
import org.lwjgl.opengl.{Display, DisplayMode}
import org.lwjgl.{Sys, LWJGLException}
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.Timer


/**
 * Main class for 3D screen.  Can be extended to define own update implementation.
 */
class Screen(preferredWidth: Int = 800, preferredHeight: Int = 600, fullScreen: Boolean = false) {

  val root: CompositeNode = new CompositeNode()

  private var running = true
  private val timer = new Timer()
  private var lastTime = 0f

  def add(node: Node) = root.add(node)
  def remove(node: Node) = root.remove(node)
  def clear() = root.clear

  private var _title: String = getClass.getSimpleName.replace("$", "")
  def title: String = _title
  def title_= (newTitle: String) {
    _title = newTitle
    Display.setTitle(newTitle)
  }

  var targetFps = 60

  /**
   * If extending this class in an object, provide a main method that parses the arguments and calls start.
   */
  def main(arguments: Array[String]) {
    config(arguments)

    start()
  }

  /**
   * Enters openGL mode and starts the application.
   */
  final def start() {
    setMode(preferredWidth, preferredHeight, fullScreen)

    timer.reset

    init()

    while (running) {
      if (Display.isCloseRequested()) stop()
      else if (Display.isActive()) {
        // The window is in the foreground
        doUpdate()
        doRender()
        Display.sync(targetFps);
      }
      else {
        // The window is not in the foreground, so give more processor time to other applications
        sleep(100)
        doUpdate()

        if (Display.isVisible() || Display.isDirty()) {
          // Only render if the window is visible or dirty
          doRender()
        }
      }

      Display.update()

      Timer.tick
    }

    Display.destroy();
  }

  private final def doUpdate() {
    val currentTime: Float = timer.getTime
    val delta = currentTime - lastTime
    lastTime = currentTime

    update(delta, currentTime)
  }

  private final def doRender() {
    glClearColor(0,0,0,0)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    render()

    glFlush
  }

  final def stop() = running = false

  protected def config(commandLineArguments: Array[String]) {}

  /**
   * Do any initialization required for the game.
   * When this method is called, the OpenGL display has already been created.
   */
  protected def init() {}

  /**
   * Update any game logic.
   * @param secondsSinceLastUpdate number of seconds since update was called last, or since the game was initialized if this is the first call.
   * @param secondsSinceStart number of seconds since the application started.
   */
  protected def update(secondsSinceLastUpdate: Float, secondsSinceStart: Float) {}

  /**
   * Do any custom OpenGL rendering.
   */
  protected def render() {}

  private def setMode(w: Int, h: Int, fullScreen: Boolean) {
    try {
      val modes = Display.getAvailableDisplayModes()
      val matchingMode = modes find (m => m.getWidth == w && m.getHeight == h)
      val mode = matchingMode match {
        case Some(m) => m
        case None => new DisplayMode(w, h)
      }

      Display.setDisplayMode(mode);
      Display.setFullscreen(fullScreen)
      Display.setTitle(title)
      Display.setVSyncEnabled(true)
      Display.create()
      glClearColor(0, 0, 0, 0)
    } catch {
      case e: LWJGLException =>
        Sys.alert("Error setting up display", e.getMessage)
        System.exit(1)
    }
  }

  private def sleep(milliseconds: Long): Unit = {
    try {
      Thread.sleep(milliseconds);
    } catch {
      case e: InterruptedException =>
    }
  }



}