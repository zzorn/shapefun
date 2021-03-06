package org.shapefun


import java.io.{FileReader, File}
import org.yaml.snakeyaml.representer.Representer
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.{TypeDescription, Yaml}
import scala.Predef._
import utils.Logging

/**
 *
 */
// TODO: Support for expressions, and anonymous functions (parse with parser combinators and create janino expression)
// TODO: Eventually, universal graphical editor
class ModelLoader {

  private val constructor = new FilterConstructor(classOf[Model])
  private val representer = new Representer()
/*
  allowType[Tome]
  allowType[Cube]
  allowType[Tube]
  allowType[Ball]
  allowType[ScatterAlong]
  allowType[Noise1to1]
  allowType[Noise1to2]
  allowType[Noise1to3]
  allowType[Abs]
*/
  def allowType[T <: AnyRef](implicit m: Manifest[T]) {
    val kind: Class[T] = m.erasure.asInstanceOf[Class[T]]
    constructor.registerType[T]
    representer.addClassTag(kind, new Tag("!" + kind.getSimpleName))
  }

  def loadModels(files: List[File]): Map[String, Model] = {
    var result = Map[String, Model]()
    files foreach {
      f => result ++= loadModels(f)
    }
    result
  }

  def loadModels(file: File): Map[String, Model] = {
    println("Loading models from " + file)

    var result = Map[String, Model]()

    constructor.source = file.getPath
    val yaml = new Yaml(constructor, representer)
    val reader = new FileReader(file)
    try {
      val tomes = yaml.loadAll(reader).iterator()
      while (tomes.hasNext) {
        val tome = tomes.next().asInstanceOf[Model]
        result += (file.getName -> tome)
      }
    }
    catch {
      case e: Exception =>
        println("Error while loading '" + file.getName + "': " + e.getMessage)
        result
    }
    finally {
      if (reader != null) reader.close()
    }

    result
  }

  class FilterConstructor(rootClass: Class[_ <: AnyRef]) extends Constructor(rootClass) with Logging {

    private var mappings: Map[String, Class[_ <: AnyRef]] = Map()

    var source = "unknown"


    def registerType[T <: AnyRef](implicit m: Manifest[T]) {

      val kind: Class[T] = m.erasure.asInstanceOf[Class[T]]
      val tag: String = "!" + m.erasure.getSimpleName
      addTypeDescription(new TypeDescription(kind, tag))
      mappings += kind.getName -> kind

      log.info("Registered tag '" + tag + "' for type '" + kind.getName + "'")
      println("Registered tag '" + tag + "' for type '" + kind.getName + "'")
    }

    override def getClassForName(name: String): Class[_] = {

      if (mappings.contains(name)) mappings(name)
      else throw FilterException(name, source)
    }

    /**
     * Exception used if non-permitted class is loaded.
     */
    case class FilterException(forbiddenClass: String, source: String)
      extends Error("The class '" + forbiddenClass + "' is not permitted, but it was used in " + source)

  }

}
