
name := "shapeflow"

version := "0.1"

scalaVersion := "2.9.1"


resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"



libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.4"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.6.4"

libraryDependencies += "log4j" % "log4j" % "1.2.16"

libraryDependencies += "org.yaml" % "snakeyaml" % "1.11-SNAPSHOT"


// JMonkey Engine 3.0 for 3D gfx
libraryDependencies ++= Seq(  // Core lib
  "com.jme3" % "jME3-core" % "3.0.0.20120209-SNAPSHOT"
  ,// Test data (TODO: Large, remove in prod!)
  "com.jme3" % "jmonkeyengine3" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jME3-testdata" % "3.0.0.20120209-SNAPSHOT"
  ,// LWJGL
  "com.jme3" % "jME3-lwjgl" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jME3-lwjgl-natives" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "lwjgl" % "3.0.0.20120209-SNAPSHOT"
  ,// Bullet physics
  "com.jme3" % "jME3-jbullet" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jbullet" % "3.0.0.20120209-SNAPSHOT"
  ,// Feature libs
  "com.jme3" % "jME3-blender" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jME3-desktop" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jME3-plugins" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "jME3-terrain" % "3.0.0.20120209-SNAPSHOT"
  ,// Ogg audio
  "com.jme3" % "jME3-jogg" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "j-ogg-oggd" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "j-ogg-vorbisd" % "3.0.0.20120209-SNAPSHOT"
  ,// Third party libs
  "com.jme3" % "jinput" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "eventbus" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "stack-alloc" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "vecmath" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "xmlpull-xpp3" % "3.0.0.20120209-SNAPSHOT"
  ,// Nifty GUI
  "com.jme3" % "jME3-niftygui" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "nifty" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "nifty-default-controls" % "3.0.0.20120209-SNAPSHOT",
  "com.jme3" % "nifty-style-black" % "3.0.0.20120209-SNAPSHOT"
)

// Parboiled for parser generation
libraryDependencies += "org.parboiled" % "parboiled-scala" % "1.0.2"