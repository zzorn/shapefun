package org.shapefun.view

import com.jme3.renderer.Camera
import com.jme3.input.controls.KeyTrigger
import com.jme3.input.{KeyInput, InputManager, ChaseCamera}
import com.jme3.math.FastMath

/**
 *
 */
// TODO: Something that doesn't capture the mouse
class RotationCameraControl(cam: Camera, inputManager: InputManager) extends ChaseCamera(cam, inputManager) {

  inputManager.addMapping("ChaseCamMoveLeft", new KeyTrigger(KeyInput.KEY_A), new KeyTrigger(KeyInput.KEY_LEFT))
  inputManager.addMapping("ChaseCamMoveRight", new KeyTrigger(KeyInput.KEY_D), new KeyTrigger(KeyInput.KEY_RIGHT))
  inputManager.addMapping("ChaseCamDown", new KeyTrigger(KeyInput.KEY_S), new KeyTrigger(KeyInput.KEY_DOWN))
  inputManager.addMapping("ChaseCamUp", new KeyTrigger(KeyInput.KEY_W), new KeyTrigger(KeyInput.KEY_UP))
  inputManager.addMapping("ChaseCamZoomIn", new KeyTrigger(KeyInput.KEY_E), new KeyTrigger(KeyInput.KEY_PGUP))
  inputManager.addMapping("ChaseCamZoomOut", new KeyTrigger(KeyInput.KEY_Q), new KeyTrigger(KeyInput.KEY_PGDN))

  rotationSpeed = 2f
  zoomSpeed = 5f

  setMinVerticalRotation(-FastMath.PI / 2f + 0.01f)
  setMaxVerticalRotation(FastMath.PI / 2f - 0.01f)

  def setRotationSpeed(speed: Float) {
    rotationSpeed = speed
  }

  def getRotationSpeed: Float = rotationSpeed

  def setZoomSpeed(speed: Float) {
    zoomSpeed = speed
  }

  def getZoomSpeed: Float = zoomSpeed


}
