
Simple 3D library for scala that wraps LWJGL.

The aim is to support creation of procedurally defined and animated shapes well.

Libraries to use:
* LWJGL for opengl
* Simplex3D for 3D math
* Possibly a separate datafun or similar library with basic scala bean style property support and support for properties that change over time, as well as loading properties and scenegraphs from files. 

Possible features to include:
* Interleaved VBO objects for fast rendering
* Basic scenegraph
* A few flexible primitives where the surfaces can be subdivided and the shape, color, texture projection, etc defined with functions
  * Subdivided box
  * Subdivided sphere
  * Lathe / subdivided cylinder / cone / capsule / extrusion
  * Terrain field
  * Skysphere (just a specialized sphere)
* Particle engine? 
* Shader support
* Lighting
* Simple collision detection
* Ray Picking
* Skeletal animation
* Interfaces for writing own texture generators, or just loading them from disk (texture generation could be done with an external library such as rasterfun).
* Impostor creation for large scenes?

Some kind of UI primitives will also be needed.
