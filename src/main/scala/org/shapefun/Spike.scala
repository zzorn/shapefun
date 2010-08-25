package org.shapefun

import org.lwjgl.{Sys, LWJGLException}
import org.lwjgl.opengl.{DisplayMode, Display}
import org.lwjgl.opengl.GL11._
import org.lwjgl.input.Keyboard

/**
 *
 */
object Spike {
  /** Desired frame time */
  private val FRAMERATE = 60


  def main(args: Array[String]) {

    val targetWidth = 800
    val targetHeight = 600

    var chosenMode: DisplayMode = Display.getDesktopDisplayMode()

    try {
      val modes = Display.getAvailableDisplayModes();

      for (val i <- 0 until modes.length) {
        println(modes(i))

        if ((modes(i).getWidth() == targetWidth) && (modes(i).getHeight() == targetHeight)) {
          chosenMode = modes(i)
        }
      }

    } catch {
      case e: LWJGLException =>
        Sys.alert("Error", "Unable to determine display modes.")
        System.exit(0)
      //e.printStackTrace
    }

    try {
      //Display.setDisplayMode(chosenMode);
      Display.setDisplayMode(new DisplayMode(targetWidth, targetHeight))
      Display.setFullscreen(false)
      Display.setTitle("An example title")
      Display.setVSyncEnabled(true)
      Display.create();
    } catch {
      case e: LWJGLException =>
        Sys.alert("Error", "Unable to create display.")
        System.exit(0);
    }



    // go on to do any initialisation of OpenGL here, for example loading textures
    // or setting up basic global settings
    glClearColor(0, 0, 0, 0);



    var gameRunning = true
    var pos = 0f

    while (gameRunning) {
      // perform game logic updates here
      pos += 0.01f;

      // render using OpenGL
      glBegin(GL_QUADS);
      glVertex3f(0, 0, pos);
      glVertex3f(1, 0, pos);
      glVertex3f(1, 1, pos);
      glVertex3f(0, 1, pos);
      glEnd();

      // now tell the screen to update
      Display.update()

      // finally check if the user has requested that the display be
      // shutdown
      if (Display.isCloseRequested()) {
        gameRunning = false;
      }
      // The window is in the foreground, so we should play the game
      else if (Display.isActive()) {
        logic();
        render();
        Display.sync(FRAMERATE);
      }
      // The window is not in the foreground, so we can allow other stuff to run and
      // infrequently update
      else {
        try {
          Thread.sleep(100);
        } catch {
          case e: InterruptedException =>
        }
        logic();

        // Only bother rendering if the window is visible or dirty
        if (Display.isVisible() || Display.isDirty()) {
          render();
        }
      }


    }
    Display.destroy();
    System.exit(0);
    
    def logic() {
// Example input handler: we'll check for the ESC key and finish the game instantly when it's pressed
      if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) {
        gameRunning = true;
      }

      // Rotate the square
//    angle += 2.0f % 360;

    }
    def render() {}
  }

}

